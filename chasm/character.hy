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
(import chasm.chat [edit respond])


(defn spawn [[name None] [coords None]] ; -> Character
  "Spawn a character from card, db, or just generated."
  (try
    (let [loaded (or (load (.join "/"[path
                                      "characters"
                                      f"{name}.json"]))
                     {})
          ; only allow to override some
          sanitised {"name" (or name (:name loaded None))
                     "appearance" (:appearance loaded None)
                     "backstory" (:backstory loaded None)
                     "voice" (:voice loaded None)
                     "traits" (:traits loaded None)
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
      (log.error e))))

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
dislikes: 'their fears and aversions'"
                      f"Story setting: {world}

Complete the character card for {name-str} whom is found in the story at {place.name}.
Give one attribute per line, with no commentary or other notes, just the updated template with the details.
Make up a brief few words for each attribute but be very specific.
"))]
    (try
      (let [details (grep-attributes kvs ["name" "appearance" "backstory" "voice" "traits" "motivation" "dislikes"])]
        (log.info f"Creating character '{(:name details)}'")
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
  "List of characters at a location"
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

(defn converse [c1 c2 [dialogue None]]
  "Carry on a conversation between two characters.
Save and recall relevant bits.")
