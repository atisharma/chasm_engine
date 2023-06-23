"
The global state.

Thing in themselves and relationships between things.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import json)
(import pathlib [Path])
(import datetime [datetime timezone])
(import string [capwords])

(import atexit)
(import sqlitedict [SqliteDict])

(import chasm.stdlib *)
(import chasm.types [Place Item Character])


;;; -----------------------------------------------------------------------------
;;; World info
;;; -----------------------------------------------------------------------------

(setv path (config "world"))

(setv world-name (-> path
                     (.split "/")
                     (last)
                     (capwords)))

(.mkdir (Path path) :parents True :exist-ok True)

(setv world (-> (Path f"{path}.txt")
                (.read-text)
                (.strip)))

(defn news []
  "Up-to-date info about the universe."
  f"It is now {(.strftime (datetime.now timezone.utc) "%H:%M, %a %d %h")}.")

;;; -----------------------------------------------------------------------------

(defn dumps [db]
  "Print a table of db."
  (for [r (db.values)]
    (print r)))

;;; -----------------------------------------------------------------------------
;;; Relationships between things
;;; -----------------------------------------------------------------------------

;; key is item name, value is its location or character name as loc:place-name or cha:character-name
;; should we have location as part of Item?

(setv geo-index {})

;;; -----------------------------------------------------------------------------
;;; Characters
;;; key is character name, value is Character
;;; -----------------------------------------------------------------------------

(setv characters (SqliteDict f"{path}/db.sqlite"
                             :tablename "characters"
                             :autocommit True
                             :encode json.dumps
                             :decode json.loads))
(.register atexit characters.close)

(defn get-character [char-name]
  (try
    (Character #* (get characters char-name))
    (except [KeyError])))

(defn set-character [char]
  (setv (get characters char.name) char)
  (.commit characters))

(defn update-character [char #** kwargs]
  "Update a character's details. You cannot change the name."
  (let [new-char { #** (._asdict char) #** kwargs}]
    (setv (get characters (:name new-char))
          (Character #** new-char))
    (.commit characters)))

;;; -----------------------------------------------------------------------------
;;; Locations
;;; key is string repr of coords, value is Location
;;; -----------------------------------------------------------------------------

(setv places (SqliteDict f"{path}/db.sqlite"
                         :tablename "places"
                         :autocommit True
                         :encode json.dumps
                         :decode json.loads))
(.register atexit places.close)

(defn get-place [coords]
  (let [key (str coords)]
    (try
      (Place #* (get places key))
      (except [KeyError]))))

(defn set-place [loc]
  (let [key (str loc.coords)]
    (setv (get places key) loc))
  (.commit places))

(defn update-place [loc #** kwargs]
  "Update a place's details. You cannot change the coordinates
(it replaces the place at those coords instead)."
  (let [new-loc { #** (._asdict loc) #** kwargs}
        key (str (:coords new-loc))]
    (setv (get places key)
          (Place #** new-loc))
    (.commit places)))

;;; -----------------------------------------------------------------------------
;;; Items
;;; key is item name, value is Item
;;; -----------------------------------------------------------------------------

(setv items (SqliteDict f"{path}/db.sqlite"
                        :tablename "items"
                        :autocommit True
                        :encode json.dumps
                        :decode json.loads))
(.register atexit items.close)

(defn get-item [item-name]
  (try
    (Item #* (get items item-name))
    (except [KeyError])))

(defn set-item [item]
  (setv (get items item.name) item)
  (.commit items))

(defn update-item [item #** kwargs]
  "Update an item's details. You cannot change the name."
  (let [new-item { #** (._asdict item) #** kwargs}]
    (setv (get items (:name new-item))
          (Item #** new-loc))
    (.commit items)))

;;; -----------------------------------------------------------------------------
;;; Dialogues
;;; -----------------------------------------------------------------------------

;; a vector db per character?

(setv dialogues {})

;;; -----------------------------------------------------------------------------
;;; Event dbs
;;; -----------------------------------------------------------------------------
