"
Functions that deal with items.
"

(defn get-items [loc]
  "List of items (names) at a location"
  (lfor [item-name item-loc] (.items state.geo-index)
        :if (= item-loc loc.name)
        item-name)) 

(defn unclaimed-items [])


(defn move [item loc-name]
  "Move an item to a location.")

(defn pick-up [item cha-name])

(defn drop [item loc-name])
