"
Functions that deal with characters.

immutable traits:
- name
- appearance
- backstory
- voice
- traits
- dislikes

mutable traits:
- coords
- inventory
- quest
- score
- memories ; db of chats? Events?
- health
- skills
- emotions

"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import random [randint choice])

(import chasm.stdlib *)
(import chasm.types [Coords Character Item at?])
(import chasm [place])

(import chasm.state [news world
                     get-item update-item
                     get-place
                     get-character set-character update-character])
(import chasm.chat [edit respond])


(setv default-character (Character
                          :name None
                          :appearance "a generic NPC"
                          :backstory "Unknown"
                          :voice "Unknown"
                          :traits "Unknown"
                          :motivation "Unknown"
                          :dislikes "Unknown"
                          :coords None
                          :quest None
                          :score None
                          :memories None
                          :health None
                          :skills None
                          :emotions None))
                          

(defn spawn [name [box #(-3 3)]]
  "Spawn at random coordinates."
  ;; TODO: save previous location (in character's state)
  (let [character (or (get-character name) (new name))
        coords (Coords (randint #* box) (randint #* box))]
    (place.extend-map coords)
    coords))

(defn gen [coords [name None]]
  "Make up some plausible character based on a name."
  (let [seed (choice "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        name-str (or name f"the character (whose name begins with '{seed}')")
        name-dict (if name {"name" name} {})
        place (get-place coords)
        place-name (if place place.name "typical place")
        kvs (-> (edit f"name: {name-str}
appearance: {name-str}'s appearance
backstory: their backstory
voice: their manner of speaking
traits: shapes their behaviour
motivation: drives their behaviour
dislikes: their fears and aversions"
                      f"Story setting: {world}

Complete the template for {name-str} whom is found in the story at {place.name}. Make up appearance and so on. Be very brief (a few words each) but very specific. Give no commentary or other notes, just the updated template with the details.")
                (.strip)
                (.split "\n"))
        details (dict (lfor kv kvs
                            (let [[k v] (.split kv ": ")]
                              (map (fn [s] (.strip s))
                                   [(.lower k) v]))))]
    (try
      (print place seed name-str)
      (Character #** (| (._asdict default-character) details name-dict {"coords" coords}))
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (print "Bad new character:" (json.dumps details :indent 4))
        (print e)))))

(defn new [place])

(defn teleport [cha coords]
  "Just set a location."
  (update-character cha :coords coords))
