"
Various types used throughout.
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

(defn is-at [coords1 coords2]
  "Equality in location. Example usage: `(at? item.coords character.coords)`."
  (and (= (:x coords1) (:x coords2))
       (= (:y coords1) (:y coords2))))

(setv Place (namedtuple "Place" ["coords"
                                 "name"
                                 "rooms"]))

(setv Item (namedtuple "Item" ["name"
                               "type"
                               "appearance"
                               "usage"
                               "owner" ; None if no character has in their inventory
                               "coords"])) ; None if it's being carried

(setv Character (namedtuple "Character" [; immutable traits
                                         "name"
                                         "appearance"
                                         "backstory"
                                         "voice"
                                         "traits"
                                         "motivation"
                                         "dislikes"
                                         ; mutable traits
                                         "coords"
                                         "quest"
                                         "score"
                                         "memories" ; db of chats? Events?
                                         "inventory"
                                         "health"
                                         "skills"
                                         "emotions"]))

;; should be an event db? sqlite table or vectordb?
(setv Event (namedtuple "Event" ["name"
                                 "facts"
                                 "coords"
                                 "description"]))

;; chats? Similarity search with threshold?
