"
Various types used throughout.
"
(require hyrule.argmove [-> ->>])

(import json)
(import collections [namedtuple])


(defn as-json [x]
  "Format a namedtuple as json."
  (-> (x._asdict)
      (json.dumps)))

(setv Location (namedtuple "Location" ["name"
                                       "facts"
                                       "coords" ; [x y], loaded as a list by json loads
                                       "location_type" ; one of [room, building, campus, city, region, country]
                                       "accessible" ; list of accessible locations
                                       "terrain"]))

(setv Item (namedtuple "Item" ["name"
                               "item_type"
                               "facts"
                               "owner" ; None if no character has in their inventory
                               "is_fixed" ; bool, is fixed in place at a location or not
                               "abilities"]))

;; should be an event db? sqlite table or vectordb?
(setv Event (namedtuple "Event" ["name"
                                 "facts"
                                 "coords"
                                 "description"]))

(setv Character (namedtuple "Character" ["inventory"
                                         "coords"
                                         "memory" ; db of chats
                                         "health"
                                         "skills"
                                         "quests"
                                         "species"
                                         "appearance"
                                         "occupation"
                                         "personality"
                                         "mental_state"
                                         "plan"
                                         "goals"
                                         "intention"]))


