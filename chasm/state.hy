"
The global state.

Thing in themselves and relationships between things.
"
(require hyrule.argmove [-> ->>])

(import json)
(import sqlitedict [SqliteDict])

(import chasm.stdlib [config slurp])
(import chasm.types [Location Item Character])


(setv path (config "world" "path"))
(setv world (->> [path "world.txt"]
                 (.join "/") 
                 (slurp)))

;;; -----------------------------------------------------------------------------
;;; Relationships between things
;;; -----------------------------------------------------------------------------

;; key is item name, value is its location or character name as loc:location-name or cha:character-name
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
;;; key is coords-str, value is Location
;;; -----------------------------------------------------------------------------

(setv locations (SqliteDict f"{path}/db.sqlite"
                            :tablename "locations"
                            :autocommit True
                            :encode json.dumps
                            :decode json.loads))

(defn get-location [coords]
  (try
    (Location #* (get locations (str coords)))
    (except [KeyError])))

(defn set-location [loc]
  (setv (get locations (str loc.coords)) loc))

(defn update-location [loc #** kwargs]
  "Update a location's details. You cannot change the coordinates."
  (let [new-loc { #** (._asdict loc) #** kwargs}]
    (setv (get locations (str (:coords new-loc)))
          (Location #** new-loc))))

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
