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

(setv Place (namedtuple "Place" ["coords"
                                 "name"
                                 "rooms"]))

(setv Item (namedtuple "Item" ["name"
                               "item_type"
                               "facts"
                               "owner" ; None if no character has in their inventory
                               "coords" ; None if it's being carried
                               "is_fixed" ; bool, is fixed in place at a location or not
                               "abilities"]))

;; TODO: reduce number of fields
(setv Character (namedtuple "Character" ["inventory"
                                         "coords"
                                         "memories" ; db of chats? Events?
                                         "health"
                                         "skills"
                                         "quests"
                                         "appearance"
                                         "occupation"
                                         "personality"
                                         "mental_state"
                                         "goals"
                                         "anti_goals"
                                         "intention"]))


;; should be an event db? sqlite table or vectordb?
(setv Event (namedtuple "Event" ["name"
                                 "facts"
                                 "coords"
                                 "description"]))

;; chats? Similarity search with threshold?
