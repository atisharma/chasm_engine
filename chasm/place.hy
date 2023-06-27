"
Functions that manage place.
"
;; TODO: the code in item.hy is cleaner due to lessons learned writing this module.
;; Incorporate those lessons here.

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import string [capwords])

(import chasm [state item character])
(import chasm.stdlib *)
(import chasm.constants [compass-directions alphanumeric character-density item-density])
(import chasm.state [news world get-place set-place update-place])
(import chasm.types [Coords Place])
(import chasm.chat [respond edit yes-no
                    msgs->dlg
                    system user assistant])


;;; -----------------------------------------------------------------------------
;;; Anything -> bool
;;; -----------------------------------------------------------------------------

(defn nearby? [coords1 coords2 [distance 1]]
  "Is coord1 within a distance of coord2 (inclusive)?"
  (and (<= (abs (- (:x coords1) (:x coords2))) distance)
       (<= (abs (- (:y coords1) (:y coords2))) distance)))

(defn [cache] accessible? [placename destination]
  "Is a destination accessible to the player?
We cache this both for performance and persistence of place characteristics."
  (let [response (respond [(system world)
                           (user f"Your place is {placename}. Would you expect to be able to reach {destination} from here in one or two moves?
Respond with only either 'Yes' or 'No'.")
                           (assistant "My single-word yes/no response is:")])]
    (or (similar response "yes")
        (in "yes" (.lower response)))))

;;; -----------------------------------------------------------------------------
;;; Place prompts -> text
;;; -----------------------------------------------------------------------------

(defn gen-name [nearby-places]
  "Make up a place from its neighbours."
  (let [seed (choice alphanumeric)
        terrain (choice ["a small building"
                         "a large building"
                         "a small outdoor space"
                         "a large outdoor space"
                         "an underground space"
                         "a space high up"])
        messages [(system f"The story's setting is: {world}")
                  (user f"Nearby places:
{nearby-places}

Your task is to generate a single name for {terrain} that you want to explore, that's different from those nearby, but that's in keeping with the story's setting. Avoid adjectives used in nearby places, be interesting and imaginative. Reply with just the name.
Examples:
'Residential Buildings'
'Mysterious Ruins'
'Junction'
'Inn'
'Architect's Office'
'Corner Shop'
'Palace'
'Nightclub'
'Small White House'
'Ship'
'Castle'
'Secret Cave'

The name should have '{seed}' in the first few letters.")]
        response (-> (respond messages :max-tokens 50))
        m (re.search r"[\"']([\w\d][\w\d ']+[\w\d])[\"']" response)]
    (-> (if m (m.group) response) 
        sstrip
        capwords)))

(defn chat-gen-description [nearby-str placename rooms-str messages [length "short"]]
  "Make up a short place description from its name."
  (let [messages [(system "Your purpose is to generate fun and exciting descriptions of places, in keeping with the information you have. Make the reader feel viscerally like they are present in the place.")
                  (user f"Story setting:\n'{world}'")
                  (assistant "I understand the story's environment. Provide some narrative leading up to now.")
                  #* messages
                  (assistant "Tell me about where the is reader now.")
                  (user f"The reader's location is 'The {placename}'.
{rooms-str}

Nearby places:
{nearby-str}

{(news)}")
                  (user f"Generate a {length}, vivid description of what the reader ('you') sees, hears, smells or touches from {placename}.")
                  (assistant f"The description of 'The {placename}' is:")]
        response (respond messages)]
    (trim-prose response)))

(defn [cache] edit-gen-description [nearby-str placename rooms-str [length "short"] [world-str world]]
  "Make up a short place description from its name."
  (let [text f"Your purpose is to generate fun and imaginative descriptions of places, in keeping with the information you have. Make the reader feel viscerally like they are present in the place.
Story setting:
'{world-str}'

Nearby places:
{nearby-str}

The protagonist's location is 'The {placename}'.
{rooms-str}"
        instruction f"Generate a {length}, vivid description of what the the protagonist sees, hears, smells and touches from {placename}. Write in the second person, using 'you'."
        response (edit text instruction)]
    (trim-prose response)))

(defn gen-facts [nearby-places placename]
  "Make up a few invariant facts about a place."
  (respond [(system world)
            (user f"Nearby places:
{nearby-places}

Place:
{placename}")
            (assistant "I understand the story's environment.")
            (user "Generate a few important facts which won't change about the place.")
            (assistant "The facts are:")]
           :max-tokens 200))

(defn gen-rooms [placename]
  "Make up some rooms for a place."
  (let [room-list (respond [(system f"{world}
Your purpose is to imagine a list of rooms you'd like to find at a place in an adventure game.")
                            (user f"I will give you a place.
If it has rooms (as for a building), list the names of those rooms, one per line. Otherwise (as for outdoors), say 'None'.

For example for 'Forest', the list is:
None

For 'Small House', the list is:
kitchen
bedroom
cellar

The place is called '{placename}'. List its rooms, if any.")]
                            ;(assistant "The list of rooms is:")]
                           :max-tokens 100)]
    (cut (->> room-list
              (itemize)
              (.split :sep "\n")
              (map capwords)
              (sieve)
              (filter (fn [x] (not (in x "None"))))
              (list))
         1 6)))

;; FIXME: it's a bit flakey
(defn guess-room [messages coords]
  "Guess the player's room."
  (let [dlg (msgs->dlg "Player" "Narrator" messages)  
        room-list (rooms coords :as-string False)
        rooms-str (.join ", " room-list)
        text f"{dlg}
The rooms the player might be in are:
{rooms-str}"]
    (if rooms-str
        (->> (edit text "Given the dialogue between player and narrator, which room is the player most likely currently in at the end of the dialogue? Choose only the most probable.")
             (best-of room-list)) 
        "")))

;;; -----------------------------------------------------------------------------
;;; Place functions
;;; -----------------------------------------------------------------------------

(defn random-coords [[box #(-3 3)]] ; -> coords
  "Extend the map at random coordinates."
  (let [coords (Coords (randint #* box) (randint #* box))]
    (extend-map coords)
    coords))

(defn rose [dx dy]
  "The word for the compass direction.
`dx` is eastings, `dy` is northings."
  (match #(dx dy)
         #(0 1)   "north"
         #(1 0)   "east"
         #(-1 0)  "west"
         #(0 -1)  "south"
         #(1 1)   "northeast"
         #(1 -1)  "southeast"
         #(-1 -1) "southwest"
         #(-1 1)  "northwest"))

(defn go [dirn coords [allow-inaccessible False] [threshold 0.75]] ; str, Coords -> Coords or None
  "Interpret a string as a change in location.
Match the string to a compass direction or a nearby place name.
Return new coords or None."
  (let [d (-> dirn (.lower) (.strip))
        x (:x coords)
        y (:y coords)
        n (inc (:y coords))
        e (inc (:x coords))
        s (dec (:y coords))
        w (dec (:x coords))
        places (nearby coords
                       :place True
                       :list-inaccessible allow-inaccessible)
        place-names (lfor p places p.name)
        place-coords (lfor p places p.coords)
        fuzzy-match-name (fuzzy-in d place-names :threshold threshold)] 
    ; remember, Coords use eastings then northings
    (let [new-coords (cond (in d ["n" "north"]) (Coords x n)
                           (in d ["ne" "northeast"]) (Coords e n)
                           (in d ["e" "east"]) (Coords e y)
                           (in d ["se" "southeast"]) (Coords e s)
                           (in d ["s" "south"]) (Coords x s)
                           (in d ["sw" "southwest"]) (Coords w s)
                           (in d ["w" "west"]) (Coords w y)
                           (in d ["nw" "northwest"]) (Coords w n)
                           fuzzy-match-name (get (dfor p places p.name p.coords) fuzzy-match-name))]
      (when (in new-coords place-coords) new-coords))))

(defn get-offset-place [coords dx dy]
  (get-place (Coords (+ (:x coords) dx) (+ (:y coords) dy))))

(defn nearby [_coords [name False] [place False] [coords False] [place-dirn False]
              [list-inaccessible False]]
  "A list of all existing [place names + directions]
in adjacent cells, accessible or not."
  (let [cx (:x _coords)
        cy (:y _coords)
        accessible-places (unless list-inaccessible (accessible _coords :min-places 3))]
    (lfor dx [-1 0 1]
          dy [-1 0 1]
          :setv nearby-place (get-offset-place _coords dx dy)
          :if (and nearby-place
                   (+ (abs dx) (abs dy))
                   (or list-inaccessible (in nearby-place accessible-places)))
          (cond name nearby-place.name
                place nearby-place
                coords nearby-place.coords
                place-dirn [nearby-place f"{nearby-place.name}, to the {(rose dx dy)}"]
                :else f"{nearby-place.name}, to the {(rose dx dy)}"))))

(defn nearby-str [coords #** kwargs]
  "A table of all existing [place names, directions]
in adjacent cells, accessible or not."
  (.join "\n" (nearby coords #** kwargs)))
  
(defn new [coords]
  "Add a description etc, item, character to a place."
  (let [near-places (nearby-str coords :list-inaccessible True)
        placename (gen-name near-places)
        rooms (gen-rooms placename)
        place (Place :coords coords
                     :name placename
                     :rooms rooms)]
    (set-place place)
    ; adding item, character has to occur *after* place has been set
    (when (< (/ (len state.items) (inc (len state.places))) item-density)
      (item.spawn coords))
    (when (< (/ (len state.characters) (inc (len state.places))) character-density)
      (character.spawn :name None :coords coords))
    place))

(defn accessible [coords * min-places]
  "A list of the accessible Places the player can move to.
If none are naturally accessible, pick a nearby few at random.
This function is not deterministic, because we ask the model to decide."
  (let [place (get-place coords)
        near-places (nearby coords :place True :list-inaccessible True)
        dests (lfor dest near-places
                    :if (and place (accessible? place.name dest.name))
                    dest)]
    (or dests
        (cut (sorted near-places
                     :key (fn [p] (hash-id p.name)))
             min-places))))

(defn accessible-coords [coords [min-places 3]]
  "A list of all existing Coords that are accessible from here."
  (lfor a (accessible coords :min-places min-places)
        a.coords))

(defn extend-map [coords]
  "Extend the map so neighbouring places exist."
  (let [cx (:x coords)
        cy (:y coords)]
    (lfor dx [-1 0 1]
          dy [-1 0 1]
          :setv _coords (Coords (+ cx dx) (+ cy dy))
          (or (get-place _coords)
              (new _coords)))))

(defn rooms [coords [as-string True]]
  (let [place (get-place coords)
        rooms-str (.join ", " place.rooms)
        room-str (if place.rooms
                     f"{place.name} has the following rooms: {rooms-str}"
                     "")]
    (if as-string
        room-str
        place.rooms)))

(defn describe [coords [messages None] [length "short"]]
  "Return a description of the location."
  (let [place (get-place coords)]
    (if place
        (if messages
            (chat-gen-description (nearby-str coords :list-inaccessible False)
                                  place.name
                                  (rooms coords)
                                  messages
                                  :length length)
            (chat-gen-description (nearby-str coords :list-inaccessible False)
                                  place.name
                                  (rooms coords)
                                  [(user "This is the first scene in the story.")]
                                  :length length))
        "I can't even begin to tell you how completely lost you are. How did you get here?")))

(defn name [coords]
  (. (get-place coords) name))
