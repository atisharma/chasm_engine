"
Functions that manage location.

* inventory
* nearby locations
  - locations at the eight directions.
* world coordinates
* terrain
* kind [room, building, campus, region]
"

(import [collections [UserList UserDict]])


(defn fmt [loc]
  "String representation ready for prompt injection.")

(defn populate [loc]
  "Create a new location. Make sure it has all fields."
  (let [default-loc {"name" None
                     "description" None
                     "coords" None
                     "terrain" None
                     "kind" None
                     "nearby" []}
        location {#** loc default-loc}]
    location))

(defn invent-description [loc]
  "Add a description etc... to a location."
  (let [description None
        terrain None
        nearby None]
    None))
