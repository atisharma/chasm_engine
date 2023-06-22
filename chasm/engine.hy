"
The main game engine.
"
(require hyrule.argmove [-> ->>])

(import functools [partial])
(import random [choice])
(import string [capwords])

(import chasm.stdlib *)
(import chasm.chat [respond true-false system user assistant])
(import chasm.state [world])


;;; -----------------------------------------------------------------------------
;;; Item prompts
;;; -----------------------------------------------------------------------------

(defn is-action [messages])

(defn use [messages item])

;;; -----------------------------------------------------------------------------
;;; Location prompts -> text
;;; -----------------------------------------------------------------------------

; ditch this?
(defn scene-msgs [location nearby-locs characters-present] ; -> msgs
  "Generate a message list describing the world and context."
  ; TODO: get location from character, use dbs
  [(system "You are running a game called *chasm* which is like a Multi-User Dungeon (MUD) or a game like Zork.
Characters in *chasm* can navigate the world, interact with the world and each other, observe the world, and interact with other characters.
Characters can pick up or drop items, use items, carry an inventory of items, and say arbitrary things to any other character.

The occasional player character will appear, as though this was a person connected online.

You can infer a goal and a purpose to *chasm* from the world information.
The storyline is affected by characters' actions but can also progress on its own in between turns.
")
   (user "I will send a long message describing the environment for the MUD (the plot, characters present, world state etc. in JSON format).
Please acknowledge you understand the request.
After that, I will give your next command.")
   (assistant "Understood.")
   (user f"World information:
{world}

The player's location:
{location}

List of characters at the location:
{characters-present}")
   (assistant "I understand the game environment.")])

(defn is-move [messages character]
  "Determine whether the player is trying to move to a valid location."
  (true-false messages
              f"The active player is: {character.name}."
              "Is the player trying to move to a location accessible to them?"))

(defn accessible-locations [nearby-locs loc]
  "Determine the accessible locations the player can move to."
  (->> (respond [(user f"World information:
{world}

The player's location:
{loc}

These are the nearby locations:
{nearby-locs}

Of the these locations, list only the subset which should reasonably be accessible to the player in one move. List one location on each line, with no bullet points or quotation marks, copying the names exactly.")
                 (assistant "These are the accessible locations:")])
      (debullet)
      (capwords)
      (map sstrip)
      (list)))

(defn name-location [nearby-locs]
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

(defn _name-location [nearby-locs [hint None]]
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

(defn describe-location [nearby-locs loc-name]
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

(defn facts-location [nearby-locs loc-name]
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
;;; Moderation prompts
;;; -----------------------------------------------------------------------------

(defn is-age-appropriate [age text]
  "Whether the text is appropriate for the age.")

(defn rewrite-for-age [age text])
