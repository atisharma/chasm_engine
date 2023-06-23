"
Functions that deal with characters.
"
(import random [randint choice])

(import chasm.stdlib *)
(import chasm.types [Coords Character])
(import chasm [place])


(defn spawn [char [box #(-3 3)]]
  "Spawn at random coordinates."
  ;; TODO: save previous location (in character's state)
  ;; in which case this function should be moved to characters.hy
  (let [coords (Coords (randint #* box) (randint #* box))]
    (place.extend-map coords)
    coords))

(defn teleport [cha]
  "Just set a location.")

(defn move [cha]
  "Move to an accessible location.")
