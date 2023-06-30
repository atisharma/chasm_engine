"
Functions that deal with characters.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import chasm.stdlib *)
(import chasm.constants [alphabet appearances default-character])
(import chasm.types [Coords Character Item at?])
(import chasm [place])

(import chasm.state [news world path username
                     characters
                     get-item update-item
                     get-place
                     get-character set-character update-character])
(import chasm.chat [edit respond
                    truncate append prepend
                    user assistant system])


(defn spawn [[name None] [coords None]] ; -> Character
  "Spawn a character from card, db, or just generated."
  (try
    (let [card-path (.join "/"[path "characters" f"{name}.json"])
          loaded (or (load card-path)
                     {})
          ; only allow to override some
          sanitised {"appearance" (:appearance loaded None)
                     "backstory" (:backstory loaded None)
                     "voice" (:voice loaded None)
                     "traits" (:traits loaded None)
                     "likes" (:likes loaded None)
                     "dislikes" (:dislikes loaded None)
                     "motivation" (:motivation loaded None)}
          coords (or coords (place.random-coords #(-1 1)))
          char (or (get-character name) (gen coords name) default-character)
          filtered (dfor [k v] (.items sanitised) :if v k v)
          character (Character #** (| (._asdict default-character)
                                      (._asdict char)
                                      filtered))]
      (when character.name
        (set-character character)
        character))
    (except [e [Exception]]
      (log.error f"Spawn failed for {name} at {coords}.")
      (log.error e)
      (when (= username name) (raise (ValueError f"Bad main character card for {name}, cannot continue. Check it's valid JSON."))))))

(defn gen [coords [name None]] ; -> Character or None
  "Make up some plausible character based on a name."
  (let [seed (choice alphabet)
        name-str (or name f"the character (whose first name begins with '{seed}')")
        name-dict (if name {"name" name} {})
        place (get-place coords)
        place-name (if place place.name "a typical place in this world")
        kvs (-> (edit f"name: '{name-str}'
appearance: '{name-str}'s appearance, {(choice appearances)}, {(choice appearances)} etc'
backstory: 'their backstory'
voice: 'their manner of speaking'
traits: 'shapes their behaviour'
motivation: 'drives their behaviour'
likes: 'their desires, wants and cravings'
dislikes: 'their fears and aversions'
skills: 'what they are particularly good at'"
                      f"Story setting: {world}

Complete the character card for {name-str} whom is found in the story at {place.name}.
Give one attribute per line, with no commentary or other notes, just the updated template with the details.
Make up a brief few words for each attribute but be very specific.
"))]
    (try
      (let [details (grep-attributes kvs ["name" "appearance" "backstory" "voice" "traits" "motivation" "likes" "dislikes" "skills"])]
        (log.info f"character/gen '{(:name details)}'")
        (when (in "the character" (:name details))
            (raise "AI is too stupid to follow instructions."))
        (Character #** (| (._asdict default-character)
                          details
                          name-dict
                          {"coords" coords})))
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (log.error "Bad new character" e)
        (log.error place)
        (log.error seed)
        (log.error name-str)
        (log.error kvs)))))

(defn describe [character [long False]]
  "A string that briefly describes the character."
  (if character
    (let [attributes (._asdict character)]
      (.pop attributes "coords")
      (if long
          (str (dfor #(k v) (.items attributes)
                     :if v
                     k v))
          f"{character.name} - {character.appearance}"))
    ""))

(defn describe-at [coords [long False]]
  "A string describing any characters at a location."
  (let [all-at (get-at coords)]
    (if all-at
        (.join "\n" ["The following characters (and nobody else) are here:"
                     #* (map (fn [c] (describe c :long long)) all-at)])
        "")))
  
(defn get-at [coords]
  "List of characters at a location, excluding player."
  (let [cs (map get-character characters)]
    (if cs
      (lfor character cs
            :if (and (at? coords character.coords)
                     (not (= character.name username)))
            character)
      []))) 

(defn move [character coords]
  "Just set a location."
  (update-character character :coords coords)
  coords)

(defn assign-quest [character quest]
  "There's a new quest in town, and character's looking to follow it."
  (update-caracter character
                   :quest quest))

(defn solve-quest [character messages] 
  "It's solved, remove it and make a record somewhere. Increment score."
  (update-caracter character
                   :score (inc character.score)
                   :quest None))

(defn develop [character dialogue]
  "Develop a character's attributes based on the dialogue."
  (let [card (user f"name: {character.name}
appearance: {character.appearance}
health: {character.health}
emotions: {character.emotions}
quest: {character.quest}
new_memory: in one short sentence, the most significant or poignant thing worth remembering from this dialogue.")
        dialogue-str (format-msgs dialogue)
        setting (system f"Story setting: {world}")
        instruction (system f"You will be given a character card for {character.name}, and the transcript of a dialogue between {character.name} and another character, for context.
Update the character card in light of the context.
Give one attribute per line, with no commentary or other notes, just the card with the updated details. Attributes can remain unaltered if there is no reason to change them. Use a brief few words for each attribute but be very specific.")
        kvs (-> (respond [instruction
                          setting
                          (user "The dialogue is as follows.")
                          (user dialogue-str)
                          (assistant "I am ready to update the character's attributes.")
                          (user "The character card to update is as follows.")
                          card]))]
    (try
      (let [details (grep-attributes kvs ["appearance" "health" "emotions" "quest" "new_memory"])
            new-memory (.pop details "new_memory" None)
            memories (if (and new-memory
                              (not (similar "the most significant or poignant thing worth remembering from this dialogue" new-memory)))
                         (append new-memory character.memories)
                         character.memories)]
        (log.info f"character/develop '{character.name}'")
        (update-character character :memories memories #** details))
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (log.error "Bad character" e)
        (log.error kvs)))))

(defn record-dialogue [dialogue]
  "Summarise the dialogue and commit it to memory.")

(defn recall-dialogue [dialogue]
  "Recall a summary of the dialogue.")
