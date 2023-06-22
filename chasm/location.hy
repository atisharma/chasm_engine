"
Functions that manage location.
"
(require hyrule.argmove [-> ->>])

(import json)
(import random [choice])
(import string [capwords])

(import chasm.stdlib *)
(import chasm.state [world get-location set-location update-location])
(import chasm.types [Coords Location])
(import chasm.chat [respond true-false system user assistant])

;;; -----------------------------------------------------------------------------
;;; Location prompts -> text
;;; -----------------------------------------------------------------------------

(defn is-move [messages accessible-locs character]
  "Determine whether the player is trying to move to a valid location."
  (true-false messages
              f"The active player is: {character.name}.
Accessible locations are:
{nearby-locs}"
              "Is the player trying to move to an accessible location?"))

(defn is-accessible [loc-name destination]
  "Is a destination accessible to the player?"
  (let [response (respond [(system f"World information: {world}")
                           (user f"Your location is {loc-name}. Would you expect to be able to reach {destination} from here in one move?
Respond with only one boolean, either 'True' or 'False'.")
                           (assistant "My single-word boolean response is:")])]
    (or (similar response "True")
        (in "true" (.lower response)))))

(defn give-name [nearby-locs]
  "Make up a location from its neighbours."
  (let [terrain (choice ["a small room"
                         "a large room"
                         "a small building"
                         "a large building"
                         "a small outdoor space"
                         "a large outdoor space"
                         "an underground space"
                         "a space high up"])
        messages [(system world)
                  (user f"Nearby, there is:
{nearby-locs}

Your task is to generate a single, interesting name for {terrain} that you want to explore, that's not one nearby. Reply with just the name.
Examples:
'Mysterious Ruins'
'Enchanted Forest'
'City Tower Block'
'Small White House'
'Crossroads'
'Ship's Hold'
'Castle Kitchen'
'Secret Cave'")]]
    (-> (respond messages :max-tokens 15)
        (sstrip)
        (capwords))))

(defn _defunct?_name [nearby-locs [hint None]] ; TODO: REMOVE once name-location is validated
  "Make up a location from its neighbours."
  (-> (respond [(system "Your purpose is to make up names of locations in keeping with the game environment.")
                (system f"Story context: {world}")
                (user "Locations may be outdoor spaces buildings, or rooms within buildings. They vary in size, so we split up very large areas (for example a forest or beach) into at most two or three smaller ones. A new location is often unlike nearby locations. If you have doubts, don't ask questions, just try.
I will you send a long message describing the environment for the game. Please acknowledge you understand the request.
After that, I will give your next command.")
                (assistant "Understood.")
                (user f"Nearby locations and their direction from here:
{nearby-locs}")
                (assistant "I understand the game environment.")
                (user "Generate a single, interesting name for a new location that the player coming from nearby would want to explore. Don't use any word already used in names of nearby locations. Reply with just the name. For example:
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

(defn describe [nearby-locs loc-name]
  "Make up a short location description from its name."
  (respond [(system "Your purpose is to generate fun and exciting descriptions of locations, in keeping with the information you have. Make the reader feel viscerally like they are present in the location.")
            (user f"World information:
{world}

Nearby locations:
{nearby-locs}

The player is located at:
{loc-name}")
            (assistant "I understand the game environment.")
            (user f"Generate a vivid description of about 100 words of what the player ('you') sees, hears, smells and touches from {loc-name}.")
            (assistant f"The description of '{loc-name}' is:")]
           :max-tokens 500))

(defn facts [nearby-locs loc-name]
  "Make up invariant facts about a location."
  (respond [(system "Your purpose is to generate fun and exciting new locations in keeping with the information you have.
")
            (user f"Nearby locations:
{nearby-locs}

Location:
{loc-name}")
            (assistant "I understand the game environment.")
            (user "Generate a few important facts which won't change about the location.")
            (assistant "The facts are:")]
           :max-tokens 150))

;;; -----------------------------------------------------------------------------
;;; Location functions
;;; -----------------------------------------------------------------------------

(defn new [coords [hint None]]
  "Add a description etc... to a location."
  (let [near-locs (nearby-str coords)
        loc-name (give-name near-locs)
        loc (Location :coords coords
                      :name loc-name
                      :events None)]
    (set-location loc)
    loc))

(defn is-nearby [coords1 coords2 [distance 1]]
  "Is coord1 within a distance of coord2 (inclusive)?"
  (all (<= (abs (- (:x coords1) (:x coords2))) distance)
       (<= (abs (- (:y coords1) (:y coords2))) distance)))

(defn get-offset [coords dx dy]
  (get-location (Coords (+ (:x coords) dx) (+ (:y coords) dy))))

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

(defn nearby-list [coords [direction True] [return-location False]]
  "A list of all existing [location names, directions]
in adjacent cells, accessible or not."
  (let [cx (:x coords)
        cy (:y coords)]
    (lfor dx (range -1 2)
          dy (range -1 2)
          :setv nearby-loc (get-offset coords dx dy)
          :if (and nearby-loc (+ (abs dx) (abs dy)))
          (cond direction (.join ", "
                                 [f"{nearby-loc.name}"
                                  ;f"at [{(+ cx dx)} {(+ cy dy)}]"
                                  f"to the {(rose dx dy)}"])
                return-location nearby-loc
                :else f"{nearby-loc.name}"))))

(defn nearby-str [coords [direction True]]
  "A table of all existing [location names, directions]
in adjacent cells, accessible or not."
  (.join "\n" (nearby-list coords :direction direction)))
  
(defn accessible [loc]
  "A list of the accessible Locations the player can move to.
If none are naturally accessible, pick one at random."
  (let [near-locs (nearby-list loc.coords :direction False :return-location True)
        dests (lfor dest near-locs
                    :if (accessible? loc.name dest.name)
                    dest)]
    (or dests (choice near-locs))))

