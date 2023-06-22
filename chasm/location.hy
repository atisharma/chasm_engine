"
Functions that manage location.
"

(import json)

(import chasm.state [get-location set-location update-location])
(import chasm.types [Coords Location])
(import chasm.engine [accessible-locations
                      name-location
                      describe-location
                      facts-location])


(defn get-items [loc]
  "List of items (names) at a location"
  (lfor [item-name item-loc] (.items state.geo-index)
        :if (= item-loc loc.name)
        item-name)) 

(defn unclaimed-items [])


(defn new [coords [hint None]]
  "Add a description etc... to a location."
  (let [near-locs (nearby coords)
        loc-name (name-location near-locs)
        loc (Location :coords coords
                      :name loc-name
                      :events None)]
    (set-location loc)
    loc))

(defn update [loc event]
  "Update a location's facts in light of something...")
  ; make a new one from the old and overwrite bits

(defn is-nearby [coords1 coords2 [distance 1]]
  "Is coord1 within a distance of coord2 (inclusive)?"
  (all (<= (abs (- (:x coords1) (:x coords2))) distance)
       (<= (abs (- (:y coords1) (:y coords2))) distance)))

(defn get-location-offset [coords dx dy]
  (get-location (Coords (+ (:x coords) dx) (+ (:y coords) dy))))

(defn rose [dx dy]
  "The word for the compass direction.
`dx` is eastings, `dy` is northings."
  (match #(dx dy)
         #(0 1)   "north"
         #(1 0)   "east"
         #(-1 0)  "west"
         #(0 -1)  "south"
         #(1 1)   "northeast"
         #(1 -1)  "southeast"
         #(-1 -1) "southwest"
         #(-1 1)  "northwest"))

(defn nearby [coords [direction True]]
  "A table of all existing [location names, directions]
in adjacent cells, accessible or not."
  (let [cx (:x coords)
        cy (:y coords)]
    (.join "\n"
           (lfor dx (range -1 2)
                 dy (range -1 2)
                 :setv nearby-loc (get-location-offset coords dx dy)
                 :if (and nearby-loc (+ (abs dx) (abs dy)))
                 (if direction
                     (.join ", "
                            [f"{nearby-loc.name}"
                             ;f"at [{(+ cx dx)} {(+ cy dy)}]"
                             f"to the {(rose dx dy)}"])
                     f"{nearby-loc.name}")))))

(defn accessible [loc]
  "A list of all locations accessible from the current location.")
  ;; generate, split
  ;; then keep those matching nearby

