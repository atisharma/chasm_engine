"
Functions that manage place.
"

(require hyrule [-> ->>])
(require hyrule [unless])

(import async-lru [alru-cache])

(import chasm [log])

(import chasm.stdlib *)
(import chasm.constants [compass-directions alphanumeric place-types place-attributes])
(import chasm.state [world get-place set-place update-place])
(import chasm.types [Coords Place])
(import chasm.chat [respond complete-json complete-lines msgs->dlg
                    system user assistant])


(defclass PlaceError [Exception])

;;; -----------------------------------------------------------------------------
;;; Anything -> bool
;;; -----------------------------------------------------------------------------

(defn nearby? [coords1 coords2 [distance 1]]
  "Is coord1 within a distance of coord2 (inclusive)?"
  (and (<= (abs (- (:x coords1) (:x coords2))) distance)
       (<= (abs (- (:y coords1) (:y coords2))) distance)))

(defn/a [(alru-cache :maxsize 1000)] accessible? [placename destination]
  "Is a destination accessible to the player?
We cache this both for performance and persistence of place characteristics."
  (let [response (await (respond [(system world)
                                  (user f"Your place is {placename}. Would you expect to be able to reach {destination} from here in one or two moves?
Respond with only either 'Yes' or 'No'.")
                                  (assistant "My single-word yes/no response is:")]))]
    (or (similar response "yes")
        (in "yes" (.lower response)))))

;;; -----------------------------------------------------------------------------
;;; Place prompts -> text
;;; -----------------------------------------------------------------------------

(defn/a chat-gen-description [nearby-str place player messages [length "very short"]]
  "Make up a short place description from its name."
  (let [messages [(system "Your purpose is to generate fun and exciting descriptions of places, in keeping with the information you have. Make the player feel viscerally like they are present in the place.")
                  (user f"Story setting:\n'{world}'")
                  (system "Next is some narrative leading up to now.")
                  #* messages
                  (system "Next follows details about {player.name}'s location.")
                  (user f"The player's location is 'The {place.name}', with the following attributes:
{place}

Nearby places:
{nearby-str}

The player is sometimes called 'user' or '{player.name}' - these refer to the same person whom is referred to in the second person by the assistant. If naming the player, only ever refer to them as {player.name}.")
                  (system f"Generate a {length}, vivid description of what the player sees, hears, smells or touches from {place.name}.")
                  (assistant f"The description of 'The {place.name}' is:")]
        response (await (respond messages))]
    (trim-prose response)))

(defn/a [(alru-cache :maxsize 1000)] gen-description [nearby-str coords [world-str world]]
  "Make up a single-paragraph place description from its name."
  (let [place (get-place coords)
        prelude f"Your purpose is to generate short, fun, imaginative descriptions of a place, in keeping with the information you have. Make the reader feel viscerally like they are present in the place. Set the scene. Write in the second person, using 'you'. Be concise. Don't mention any people or characters that may be present here, concentrate on things that won't change. Write in plain, unformatted text.
Generate a short paragraph of vivid description of what the the protagonist sees, hears, smells or touches from {place.name}."
        context f"Story setting:
'{world-str}'
Nearby places:
{nearby-str}
The protagonist's new location is 'The {place.name}', with attributes:
{place}"
        instruction "Now, generate the description."
        response (await (respond
                          [(system prelude)
                           (user context)
                           (user instruction)
                           (assistant f"The description of 'The {place.name}' is:")]
                          :max-tokens 100))]
    (.join "\n\n"
           [f"**{place.name}**"
            (-> response
                (.replace "\"" "")
                trim-prose)])))

(defn/a gen-json [nearby-places]
  "Make up a place from its neighbours."
  (let [seed (choice alphanumeric)
        template f"{{
    \"name\": \"an imaginative and original place name\",
    \"appearance\": \"a few keywords\",
    \"atmosphere\": \"a few keywords\",
    \"terrain\": \"the terrain\"
}}"
        context f"Story setting: {world}
{(or (+ "Nearby are " nearby-places) "")}"
        instruction f"Complete the template for a place you want to explore, that's distinct from those nearby, but that's in keeping with the story's setting. Avoid adjectives used in nearby places, be interesting and imaginative.
Example names would include: 'Residential Buildings', 'Mysterious Ruins', 'Junction', 'Inn', 'Architect's Office', 'Corner Shop', 'Palace', 'Nightclub', 'Small White House', 'Ship', 'Castle', 'Secret Cave'.
The name should have {seed} in the first few letters. The place might be {(choice place-types)}."
        details (await (complete-json
                         :template template
                         :context context
                         :instruction instruction))]
    (when (and details (:name details None))
      {"name" (capwords (re.sub r"^[Tt]he " "" (:name details)))
       "appearance" (:appearance details None)
       "atmosphere" (:atmosphere details None)
       "terrain" (:terrain details None)})))

(defn/a gen-lines [nearby-places]
  "Make up a place from its neighbours."
  (let [seed (choice alphanumeric)
        context f"The story's setting is: {world}
Nearby places: {nearby-places}"
        template "name: substitute an imaginative and original place name
appearance: a few keywords to describe
atmosphere: a few keywords
terrain: the terrain"
        instruction f"Complete the template for a place you want to explore, that's distinct from those nearby, but that's in keeping with the story's setting. Avoid adjectives used in nearby places; be interesting and imaginative.
Example names would include: 'Residential Buildings', 'Mysterious Ruins', 'Junction', 'Inn', 'Architect's Office', 'Corner Shop', 'Palace', 'Nightclub', 'Small White House', 'Ship', 'Castle', 'Secret Cave'.
The name should have {seed} in the first few letters. The place might be {(choice place-types)}."
        details (await (complete-lines
                         :context context
                         :template template
                         :instruction instruction
                         :attributes place-attributes))]
    (when (and (:name details None)
               (< (len (.split (:name details ""))) 4))
      details)))

(defn/a gen-rooms [place-dict]
  "Make up some rooms for a place."
  (let [room-list (await (respond [(system f"Your purpose is to imagine a list of rooms you'd like to find at a place in an adventure game.
{world}
I will give you a place.
If it has rooms (as for a building), list the names of those rooms, one per line. Otherwise (as for outdoors), say 'None'.

For example for 'Forest', the list is:
None

For 'Small House', the list is:
kitchen
bedroom
cellar")

                                   (user "The place is:
{place-dict}

Now list its rooms, if any.")]
                                  :max-tokens 100))]
    (cut (->> room-list
              (debullet)
              (.split :sep "\n")
              (map capwords)
              (sieve)
              (filter (fn [x] (not (in x "None"))))
              (list))
         1 6)))

;; FIXME: it's a bit flaky
(defn/a guess-room [messages coords]
  "Guess the player's room."
  (let [dlg (await (msgs->dlg "Player" "Narrator" messages))  
        room-list (rooms coords :as-string False)
        rooms-str (.join ", " room-list)]
    (if rooms-str
        (->> (await (respond [(system "Given the dialogue between player and narrator, which room is the player most likely currently in at the end of the dialogue? Choose only the most probable.")
                              (user dlg)
                              (user "The rooms the player might be in are: {rooms-str}")]))
             (best-of room-list))
        "")))

;;; -----------------------------------------------------------------------------
;;; Place functions
;;; -----------------------------------------------------------------------------

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

(defn/a go [dirn coords [allow-inaccessible False] [threshold 0.8]] ; str, Coords -> Coords or None
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
        places (await (nearby coords
                              :place True
                              :list-inaccessible allow-inaccessible))
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

(defn/a nearby [_coords [name False] [place False] [coords False] [place-dirn False] [list-inaccessible False]]
  "A list of all existing [place names + directions] in adjacent cells, accessible or not."
  (let [cx (:x _coords)
        cy (:y _coords)
        accessible-places (unless list-inaccessible (await (accessible _coords :min-places 4)))]
    (lfor dx [-1 0 1]
          dy [-1 0 1]
          :setv nearby-place (get-offset-place _coords dx dy)
          :if (and nearby-place
                   (+ (abs dx) (abs dy)) ; not the same place
                   (or list-inaccessible (in nearby-place accessible-places)))
          (cond name nearby-place.name
                place nearby-place
                coords nearby-place.coords
                place-dirn [nearby-place f"{nearby-place.name}, to the {(rose dx dy)}"]
                :else f"{nearby-place.name}, to the {(rose dx dy)}"))))

(defn/a nearby-str [coords #** kwargs]
  "A table of all existing [place names, directions]
in adjacent cells, accessible or not."
  (.join "\n" (await (nearby coords #** kwargs))))
  
(defn/a new [coords]
  "Add a description etc, item, character to a place."
  ; TODO: consider pre-generating a long list of names, to ensure uniqueness.
  (let [near-places (.join ", " (await (nearby coords :list-inaccessible True :name True)))
        details (await (gen-lines near-places))]
    (if (and details (:name details None))
        (let [m (->> (:name details)
                     (re.search r"([\w ]+)"))
              name (when m (-> m
                               (.groups)
                               (first)
                               (sstrip)
                               (capwords)))
              place (Place :coords coords
                           :name name
                           :rooms (await (gen-rooms details))
                           :atmosphere (:atmosphere details None)
                           :appearance (:appearance details None)
                           :terrain (:terrain details None))]
          (log.info f"place/new: {place.name} @ {coords}")
          (set-place place))
      (log.error f"place/new: generation failed @ {coords}\n{near-places}\n-> {details}"))))

(defn/a accessible [coords * min-places]
  "A list of the accessible Places the player can move to.
If none are naturally accessible, pick a nearby few at random.
This function is not deterministic, because we ask the model to decide."
  (let [place (get-place coords)
        near-places (await (nearby coords :place True :list-inaccessible True))
        dests (lfor dest near-places
                    (when (and place (await (accessible? place.name dest.name)))
                          dest))]
    (or (list (sieve dests))
        (cut (sorted near-places
                     :key (fn [p] (hash-id p.name)))
             min-places))))

(defn/a accessible-coords [coords [min-places 4]]
  "A list of all existing Coords that are accessible from here."
  (lfor a (await (accessible coords :min-places min-places))
        a.coords))

(defn/a extend-map [coords]
  "Extend the map so neighbouring places exist."
  (let [cx (:x coords)
        cy (:y coords)
        places (lfor dx [-1 0 1]
                     dy [-1 0 1]
                     :setv _coords (Coords (+ cx dx) (+ cy dy))
                     (or (get-place _coords)
                         (await (new _coords))
                         (await (new _coords))))]
    (unless (get-place coords)
      (raise (PlaceError f"place/extend-map: unable to generate at {coords}")))))
    
(defn rooms [coords [as-string True]]
  (let [place (get-place coords)
        rooms-str (.join ", " place.rooms)
        room-str (if place.rooms
                     f"{place.name} has the following rooms: {rooms-str}"
                     "")]
    (if as-string
        room-str
        place.rooms)))

(defn/a describe [player [messages None] [length "very short"]]
  "Return a description of the location."
  (let [coords player.coords
        place (get-place coords)
        near-str (.join "; " (await (nearby coords :list-inaccessible False :name True)))]
    (if messages
        (await (chat-gen-description near-str
                                     place
                                     player
                                     messages
                                     :length length))
        (await (gen-description near-str (str coords))))))

(defn name [coords]
  (. (get-place coords) name))
