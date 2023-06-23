"
The global state.

Thing in themselves and relationships between things.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import json)
(import pathlib [Path])
(import sqlitedict [SqliteDict])

(import chasm.stdlib [config slurp])
(import chasm.types [Place Item Character])


(setv path (config "world"))
(.mkdir (Path path) :parents True :exist-ok True)
(setv world (.strip (slurp f"{path}.txt")))

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

(defn get-character [char-name]
  (try
    (Character #* (get characters char-name))
    (except [KeyError])))

(defn set-character [char]
  (setv (get characters char.name) char))

(defn update-character [char #** kwargs]
  "Update a character's details. You cannot change the name."
  (let [new-char { #** (._asdict char) #** kwargs}]
    (setv (get characters (:name new-char))
          (Character #** new-char))))

;;; -----------------------------------------------------------------------------
;;; Locations
;;; key is string repr of coords, value is Location
;;; -----------------------------------------------------------------------------

(setv places (SqliteDict f"{path}/db.sqlite"
                         :tablename "places"
                         :autocommit True
                         :encode json.dumps
                         :decode json.loads))

(defn get-place [coords]
  (let [key (str coords)]
    (try
      (Place #* (get places key))
      (except [KeyError]))))

(defn set-place [loc]
  (let [key (str loc.coords)]
    (setv (get places key) loc)))

(defn update-place [loc #** kwargs]
  "Update a place's details. You cannot change the coordinates
(it replaces the place at those coords instead)."
  (let [new-loc { #** (._asdict loc) #** kwargs}
        key (str (:coords new-loc))]
    (setv (get places key)
          (Place #** new-loc))))

;;; -----------------------------------------------------------------------------
;;; Items
;;; key is item name, value is Item
;;; -----------------------------------------------------------------------------

(setv items (SqliteDict f"{path}/db.sqlite"
                        :tablename "items"
                        :autocommit True
                        :encode json.dumps
                        :decode json.loads))

(defn get-item [item-name]
  (try
    (Item #* (get items item-name))
    (except [KeyError])))

(defn set-item [item]
  (setv (get items item.name) item))

(defn update-item [item #** kwargs]
  "Update an item's details. You cannot change the name."
  (let [new-item { #** (._asdict item) #** kwargs}]
    (setv (get items (:name new-item))
          (Item #** new-loc))))

;;; -----------------------------------------------------------------------------
;;; Dialogues
;;; -----------------------------------------------------------------------------

;; a vector db per character?

(setv dialogues {})

;;; -----------------------------------------------------------------------------
;;; Event dbs
;;; -----------------------------------------------------------------------------
