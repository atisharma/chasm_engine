"
The global (mostly) mutable state.

Thing in themselves and relationships between things.
"
(require hyrule.argmove [-> ->>])

(import chasm_engine [log])

(import json)
(import pathlib [Path])
(import collections [deque])

(import atexit)
(import sqlitedict [SqliteDict])
(import sqlite3 [OperationalError])
; consider using diskcache

(import chasm_engine.stdlib *)
(import chasm_engine.types [Place Item Character Coords])


;;; -----------------------------------------------------------------------------
;;; World info
;;; -----------------------------------------------------------------------------

(setv path (config "world"))
(.mkdir (Path path)
        :parents True
        :exist-ok True)

(setv world-name (-> path
                     (.split "/")
                     (last)
                     (capwords)))

(setv world (-> (Path f"{path}.txt")
                (.read-text)
                (.strip)))

(log.info f"{world-name}")

;;; -----------------------------------------------------------------------------
;;; db functions
;;; -----------------------------------------------------------------------------

(defn dumps [db]
  "Print a table of db."
  (for [r (db.values)]
    (print r)))

(defn indented-json [x]
  (json.dumps x :indent 4))

(defn get-table [tablename [db "world"] [encoder indented-json] [decoder json.loads]]
  "Make a table in the database."
  (let [t (SqliteDict f"{path}/{db}.sqlite"
                      :tablename tablename
                      :autocommit True
                      :encode encoder
                      :decode decoder)]
    (.register atexit t.close)
    t))

(defn safe-commit [table]
  "If database is locked, shrug and carry on.
But you're probably using autocommit anyway."
  (try
    (.commit table)
    (except [OperationalError])))
  
;;; -----------------------------------------------------------------------------
;;; Characters
;;; key is character name, value is Character
;;; -----------------------------------------------------------------------------

(setv characters (get-table "characters"))

(defn character-key [char-name]
  "First name, lowercase, no funny business."
  (->> char-name
       (.split)
       (first)
       (re.sub r"\W+" "")
       (.lower)))

(defn get-character [char-name]
  (log.debug f"Getting character {char-name}.")
  (when char-name
    (try
      (Character #** (get characters (character-key char-name)))
      (except [KeyError]))))

(defn set-character [char]
  (log.debug f"Setting character {char.name}.")
  (setv (get characters (character-key char.name)) (dict (sorted (.items (._asdict char)))))
  char)

(defn update-character [char #** kwargs]
  "Update a character's details. You cannot change the name."
  (log.debug f"Updating character {char.name}, {kwargs}.")
  (set-character (Character #** (| (._asdict char) kwargs))))

(defn delete-character [char-name]
  "Completely remove a character."
  (log.debug f"Deleting character {char-name}.")
  (.pop characters (character-key char-name)))

(defn get-characters []
  (gfor c characters (get-character c)))

(defn len-characters []
  (len characters))
  
;;; -----------------------------------------------------------------------------
;;; Locations
;;; key is string repr of coords, value is Location
;;; -----------------------------------------------------------------------------

(setv places (get-table "places"))

(defn get-place [coords]
  (let [key (str coords)]
    (try
      (Place #** (get places key))
      (except [KeyError]))))

(defn set-place [loc]
  (let [key (str loc.coords)]
    (setv (get places key) (dict (sorted (.items (._asdict loc))))))
  loc)

(defn update-place [loc #** kwargs]
  "Update a place's details. You cannot change the coordinates
(it replaces the place at those coords instead)."
  (log.debug f"Updating place {loc.name}, {kwargs}.")
  (set-place (Place #** (| (._asdict loc) kwargs))))

(defn delete-place [loc]
  "Completely remove a place."
  (let [key (str coords)
        p (get-place coords)]
    (log.debug f"Deleting place {p.name}.")
    (.pop places key)))
  
(defn len-places []
  (len places))
  
(defn get-places []
  (gfor p places (get-place p)))

(defn random-coords []
  "Return a random place (usually to spawn at)."
  (. (choice (list (get-places))) coords))

;;; -----------------------------------------------------------------------------
;;; Items
;;; key is item name, value is Item
;;; -----------------------------------------------------------------------------

(setv items (get-table "items"))

(defn get-item [item-name]
  (when item-name
    (try
      (Item #** (get items item-name))
      (except [KeyError]))))

(defn set-item [item]
  (setv (get items item.name) (dict (sorted (.items (._asdict item)))))
  item)

(defn update-item [item #** kwargs]
  "Update an item's details. You cannot change the name."
  (log.debug f"Updating item {item.name}, {kwargs}.")
  (set-item (Item #** (| (._asdict item) kwargs))))

(defn delete-item [item-name]
  "Completely remove an item."
  (log.debug f"Deleting item {item-name}.")
  (.pop items item-name))
  
(defn len-items []
  (len items))

(defn get-item-names []
  (.keys items))

(defn get-items []
  (gfor i items (get-item i)))

;;; -----------------------------------------------------------------------------
;;; narrative
;;; key is player name
;;; -----------------------------------------------------------------------------

(setv narratives (get-table "narratives"))

(defn get-narrative [player-name]
  (when player-name
    (try
      (get narratives (character-key player-name))
      (except [KeyError]
        []))))

(defn set-narrative [messages player-name]
  (setv (get narratives (character-key player-name)) messages)
  messages)

(defn delete-narrative [player-name]
  "Completely remove a narrative."
  (log.debug f"Deleting narrative {player-name}.")
  (.pop narratives (character-key player-name)))
  
;;; -----------------------------------------------------------------------------
;;; accounts
;;; key is player name
;;; -----------------------------------------------------------------------------

(setv accounts (get-table "accounts"))

(defn get-account [player-name]
  (log.debug f"Getting account {player-name}.")
  (when player-name
    (try
      (get accounts (character-key player-name))
      (except [KeyError]))))

(defn set-account [account player-name]
  (log.debug f"Setting account {player-name}.")
  (setv (get accounts (character-key player-name)) account)
  account)

(defn update-account [player-name #** kwargs]
  "Update a player's details. You cannot change the name."
  (log.debug f"Updating account {player-name}, {kwargs}.")
  (let [account (or (get-account player-name) {})]
    (set-account (| account kwargs) player-name)))

(defn delete-account [player-name]
  "Completely remove an account."
  (log.debug f"Deleting account {player-name}.")
  (.pop accounts (character-key player-name))
  (delete-narrative player-name))

(defn get-accounts []
  (gfor p accounts (get-account p)))
