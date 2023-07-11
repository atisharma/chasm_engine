"
The global (mostly) mutable state.

Thing in themselves and relationships between things.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import json)
(import pathlib [Path])
(import datetime [datetime timezone])

(import atexit)
(import sqlitedict [SqliteDict])
; consider using diskcache

(import chasm.stdlib *)
(import chasm.types [Place Item Character])


;;; -----------------------------------------------------------------------------
;;; World info
;;; -----------------------------------------------------------------------------

(setv path (config "world"))
(setv username (config "name"))

(setv world-name (-> path
                     (.split "/")
                     (last)
                     (capwords)))

; create world folder and subfolders
(.mkdir (Path (.join "/" [path "characters"]))
        :parents True
        :exist-ok True)
(.mkdir (Path (.join "/" [path "narratives"]))
        :parents True
        :exist-ok True)

(setv world (-> (Path f"{path}.txt")
                (.read-text)
                (.strip)))

(defn news []
  "Up-to-date info about the universe."
  f"It is {(.strftime (datetime.now timezone.utc) "%H:%M, %a %d %h")}.")

(defn now []
  "Up-to-date info about the universe."
  (.strftime (datetime.now timezone.utc) "%H:%M, %a %d %h"))

;;; -----------------------------------------------------------------------------
;;; db functions
;;; -----------------------------------------------------------------------------

(defn dumps [db]
  "Print a table of db."
  (for [r (db.values)]
    (print r)))

(defn get-table [tablename]
  "Make a table in the database."
  (let [t (SqliteDict f"{path}/{world-name}.sqlite"
                      :tablename tablename
                      :autocommit True
                      :encode (fn [x] (json.dumps x :indent 4))
                      :decode json.loads)]
    (.register atexit t.close)
    t))

;;; -----------------------------------------------------------------------------
;;; Characters
;;; key is character name, value is Character
;;; -----------------------------------------------------------------------------

(setv characters (get-table "characters"))

(defn character-key [char-name]
  "First name, lowercase."
  (.lower (first (.split char-name))))

(defn get-character [char-name]
  (log.debug f"Recalling character {char-name}.")
  (when char-name
    (try
      (Character #** (get characters (character-key char-name)))
      (except [KeyError]))))

(defn set-character [char]
  (log.debug f"Setting character {char.name}.")
  (setv (get characters (character-key char.name)) (dict (sorted (.items (._asdict char)))))
  (.commit characters)
  char)

(defn update-character [char #** kwargs]
  "Update a character's details. You cannot change the name."
  (log.debug f"Updating character {char.name}, {kwargs}.")
  (set-character (Character #** (| (._asdict char) kwargs))))

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
  (.commit places)
  loc)

(defn update-place [loc #** kwargs]
  "Update a place's details. You cannot change the coordinates
(it replaces the place at those coords instead)."
  (log.debug f"Updating place {loc.name}, {kwargs}.")
  (set-place (Place #** (| (._asdict loc) kwargs))))

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
  (.commit items)
  item)

(defn update-item [item #** kwargs]
  "Update an item's details. You cannot change the name."
  (log.debug f"Updating item {item.name}, {kwargs}.")
  (set-item (Item #** (| (._asdict item) kwargs))))

;;; -----------------------------------------------------------------------------
;;; Dialogues
;;; -----------------------------------------------------------------------------

;; a vector db per character? Or just summarisation? Generate on the fly?

;;; -----------------------------------------------------------------------------
;;; Event dbs
;;; -----------------------------------------------------------------------------

(setv events (get-table "events"))

(defn get-event [key]
  (when key
    (try
      (Event #** (get events key))
      (except [KeyError]))))

(defn set-event [event]
  (setv (get events event.time) (dict (sorted (.items (._asdict event)))))
  (.commit events)
  event)

(defn update-event [event #** kwargs]
  "Update an event's details. You cannot change the time."
  (log.debug f"Updating event {event.time}, {kwargs}.")
  (set-event (Event #** (| (._asdict event) kwargs))))
