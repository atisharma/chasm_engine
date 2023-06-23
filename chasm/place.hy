"
Functions that manage place.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import hyrule [inc dec])
(import json)
(import re)
(import random [choice])
(import string [capwords])

(import chasm.stdlib *)
(import chasm.state [world get-place set-place update-place])
(import chasm.types [Coords Place])
(import chasm.chat [respond true-false system user assistant])

;;; -----------------------------------------------------------------------------
;;; Place prompts -> text
;;; -----------------------------------------------------------------------------

(defn is-move [messages accessible-places placename character]
  "Determine whether the player is trying to move to a valid place."
  (true-false messages
              f"The active player is: {character.name}.
Accessible places are:
{nearby-places}"
              "Is the player trying to leave {placename} and move to an accessible place?"))

(defn is-accessible [placename destination]
  "Is a destination accessible to the player?"
  (let [response (respond [(system world)
                           (user f"Your place is {placename}. Would you expect to be able to reach {destination} from here in one or two moves?
Respond with only either 'Yes' or 'No'.")
                           (assistant "My single-word yes/no response is:")])]
    ;(print placename "->" destination response)
    (or (similar response "yes")
        (in "yes" (.lower response)))))

(defn gen-name [nearby-places]
  "Make up a place from its neighbours."
  (let [terrain (choice ["a small building"
                         "a large building"
                         "a small outdoor space"
                         "a large outdoor space"
                         "an underground space"
                         "a space high up"])
        messages [(system f"The story's setting is: {world}")
                  (user f"Nearby places:
{nearby-places}

Your task is to generate a single, interesting name for {terrain} that you want to explore, that's not one nearby, in keeping with the story's setting. Avoid adjectives used in nearby places. Reply with just the name.
Examples:
'Mysterious Ruins'
'Enchanted Forest'
'Block of flats'
'Corner shop'
'Small White House'
'Crossroads'
'Ship'
'Castle'
'Secret Cave'")]
        response (-> (respond messages :max-tokens 15))
        m (re.search r"[\"']([\w\d][\w\d ']+[\w\d])[\"']" response)]
    (-> (if m (m.group) response) 
        sstrip
        capwords)))

(defn _defunct?_name [nearby-places [hint None]] ; TODO: REMOVE once name-place is validated
  "Make up a place from its neighbours."
  (-> (respond [(system f"{world}
Your purpose is to make up names of places in keeping with the game environment.")
                (user "Places may be outdoor spaces buildings, or rooms within buildings. They vary in size, so we split up very large areas (for example a forest or beach) into at most two or three smaller ones. A new place is often unlike nearby places. If you have doubts, don't ask questions, just try.
I will you send a long message describing the environment for the game. Please acknowledge you understand the request.
After that, I will give your next command.")
                (assistant "Understood.")
                (user f"Nearby places and their direction from here:
{nearby-places}")
                (assistant "I understand the game environment.")
                (user "Generate a single, interesting name for a new place that the player coming from nearby would want to explore. Don't use any word already used in names of nearby places. Reply with just the name. For example:
'The Village'
'Kitchen'
'Grove'
'Mysterious Cove'
'Cathedral'
'Crypt'
'Beach'
")]
                ;(assistant "The new generated name is:")]
               :max-tokens 15)
               ;:top-p 0.25)
      (sstrip)
      (capwords)))

(defn gen-description [nearby-places placename [paragraphs 1]]
  "Make up a short place description from its name."
  (let [length (if (in paragraphs [1 "one"])
                   "no more than a single paragraph"
                   f"exactly {paragraphs} paragraphs")
        messages [(system "Your purpose is to generate fun and exciting descriptions of places, in keeping with the information you have. Make the reader feel viscerally like they are present in the place.")
                  (user f"Story setting:
'{world}'

Nearby places:
{nearby-places}

The reader's location is '{placename}'.")
                  (assistant "I understand the story's environment.")
                  (user f"Generate a vivid description of {length} of what the reader ('you') sees, hears, smells and touches from {placename}.")
                  (assistant f"The description of '{placename}' is:")]
        response (respond messages :max-tokens (* (inc paragraphs) 150))
        paras (-> response (.strip) (.split "\n\n") (sieve) (list))]
    (.join "\n\n"
           (if (in (last (last paras)) ".?!")
               paras
               (cut paras -1)))))

(defn gen-facts [nearby-places placename]
  "Make up invariant facts about a place."
  (respond [(system world)
            (user f"Nearby places:
{nearby-places}

Place:
{placename}")
            (assistant "I understand the story's environment.")
            (user "Generate a few important facts which won't change about the place.")
            (assistant "The facts are:")]
           :max-tokens 150))

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

(defn is-nearby [coords1 coords2 [distance 1]]
  "Is coord1 within a distance of coord2 (inclusive)?"
  (and (<= (abs (- (:x coords1) (:x coords2))) distance)
       (<= (abs (- (:y coords1) (:y coords2))) distance)))

(defn get-offset-place [coords dx dy]
  (get-place (Coords (+ (:x coords) dx) (+ (:y coords) dy))))

(defn nearby-list [coords [direction True] [return-place False]]
  "A list of all existing [place names, directions]
in adjacent cells, accessible or not."
  (let [cx (:x coords)
        cy (:y coords)]
    (lfor dx (range -1 2)
          dy (range -1 2)
          :setv nearby-place (get-offset-place coords dx dy)
          :if (and nearby-place (+ (abs dx) (abs dy)))
          (cond direction (.join ", "
                                 [f"{nearby-place.name}"
                                  ;f"at [{(+ cx dx)} {(+ cy dy)}]"
                                  f"to the {(rose dx dy)}"])
                return-place nearby-place
                :else f"{nearby-place.name}"))))

(defn nearby-str [coords [direction True]]
  "A table of all existing [place names, directions]
in adjacent cells, accessible or not."
  (.join "\n" (nearby-list coords :direction direction)))
  
(defn new [coords]
  "Add a description etc... to a place."
  (let [near-places (nearby-str coords)
        placename (gen-name near-places)
        rooms (gen-rooms placename)
        place (Place :coords coords
                     :name placename
                     :rooms rooms)]
    (set-place place)
    place))

(defn accessible [coords]
  "A list of the accessible Places the player can move to.
If none are naturally accessible, pick a nearby one at random."
  (let [place (get-place coords)
        near-places (nearby-list coords :direction False :return-place True)
        dests (lfor dest near-places
                    :if (and place (accessible? place.name dest.name))
                    dest)]
    (or dests (choice near-places))))

(defn extend-map [coords]
  "Extend the map so neighbouring places exist."
  (let [cx (:x coords)
        cy (:y coords)]
    (lfor dx (range -1 2)
          dy (range -1 2)
          :setv _coords (Coords (+ cx dx) (+ cy dy))
          (or (get-place _coords)
              (new _coords)))))

(defn describe [coords [paragraphs 1]]
  "Return a description of the location."
  (let [place (get-place coords)]
    (if place
        (gen-description (nearby-str coords) place.name :paragraphs paragraphs)
        "You are completely lost. How did you get here?")))
