"
Various types used throughout.

N.B. don't change the order of attributes, because the serialisation is order-dependent.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import json)
(import collections [namedtuple])


(defn as-json [x]
  "Format a namedtuple as a json string with fields."
  (-> (x._asdict)
      (json.dumps)))

(defn Coords [x y]
  "Just a dict of eastings and northings."
  {"x" x "y" y})

(defn at? [coords1 coords2]
  "Equality in location. Example usage: `(at? item.coords character.coords)`."
  (and coords1 coords2
       (= (:x coords1) (:x coords2))
       (= (:y coords1) (:y coords2))))

(setv Place (namedtuple "Place" ["coords"
                                 "name"
                                 "rooms"
                                 "appearance"
                                 "atmosphere"
                                 "terrain"]))

; consider adding provenance
(setv Item (namedtuple "Item" ["name"
                               "type"
                               "appearance"
                               "usage"
                               "owner" ; None if no character has in their inventory
                               "coords"])) ; None if it's being carried

(setv mutable-character-attributes ["name" ; maybe... you discover their surname?
                                    "appearance"
                                    "health"
                                    "emotions"
                                    "objectives"
                                    "destination"]

      initial-character-attributes ["appearance"
                                    "name"
                                    "gender"
                                    "backstory"
                                    "voice"
                                    "traits"
                                    "motivation"
                                    "likes"
                                    "dislikes"
                                    "skills"
                                    "occupation"
                                    "objectives"])
      
(setv Character (namedtuple "Character" (list (set [#* initial-character-attributes
                                                    #* mutable-character-attributes
                                                    "memories"
                                                    "coords"
                                                    "score"]))))

; basically news items
(setv Event (namedtuple "Event" ["time"
                                 "place"
                                 "coords"
                                 "characters"
                                 "point"
                                 "classification"]))

;; chats? Similarity search with threshold?
