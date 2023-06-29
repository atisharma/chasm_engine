"
Functions that deal with items.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import string [capwords])

(import chasm.stdlib *)
(import chasm.constants [alphanumeric full-inventory-messages])
(import chasm [place])
(import chasm.types [Item Coords at?])
(import chasm.state [news world get-item set-item update-item items])
(import chasm.chat [edit respond])


;; TODO: modify / damage / destroy / use items

(defn gen [place]
  "Make up some fantastical item."
  (let [seed (choice alphanumeric)
        kvs (-> (edit f"name: item name (has '{seed}' in the first few letters)
type: item type
appearance: item's appearance
usage: what the item does"
                      f"Story setting: {world}

Complete the template for a single portable object you would expect to find in the {place.name}.
Give one attribute per line, with no commentary or other notes, just the updated template with the details.
Make up a name, type, appearance, usage.
Write a very short sentence for appearance and another for usage. Be very specific."))]
    (try
      (let [details (grep-attributes kvs ["name" "appearance" "type" "usage"])]
        (log.info f"Creating item '{(:name details)}'")
        (when (in "item name" (:name details))
            (raise "AI is too stupid to follow instructions."))
        (Item #** (| {"type" "object"
                      "appearance" "Looks like you'd expect."
                      "usage" "Usage unknown."}
                     details)
              :coords place.coords
              :owner None))
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (log.error "item/gen: Bad new item" e)
        (log.error place)
        (log.error seed)
        (log.error kvs)))))

(defn spawn [coords]
  "Invent a new item from a place name, store it and return it.
None if the place doesn't exist."
  (let [p (place.get-place coords)]
    (when p
      (let [item (gen p)]
        (when (and item (not (in item.name (.keys items))))
          (set-item item))))))

(defn get-items [coords]
  "List of (unowned) items at a location"
  (lfor item (map get-item items)
        :if (at? coords item.coords)
        item)) 

(defn describe-at [coords]
  "The prosaic version of get-items."
  (let [items (get-items coords)
        items-str (.join ", " (lfor i items i.name))]
    (if items
        f"Only the following portable items are here, which may be taken: {items-str}."
        "There are no portable items here.")))

(defn get-desc [item-name] ; -> str
  "Name, type and appearance of the item."
  (let [i (get-item item-name)]
    (if i
        f"{i.name} ({i.type}) - {i.appearance} {i.usage}"
        "")))

(defn unclaimed-items []
  "List of all items (globally) without an owner.
If you just want those at a location, use `get-items`."
  (lfor item (map get-item items)
        :if (not item.owner)
        item))

(defn inventory [owner]
  "List of items with a specific owner."
  (list
    (filter (fn [i] (= i.owner owner.name))
            (map get-item items))))

(defn describe-inventory [character]
  "The prosaic version of get-items for an inventory."
  (let [items (inventory character)
        items-str (.join "\n" (lfor i items f"- {(get-desc i.name)}"))]
    (if items-str
        f"You're carrying these items:\n{items-str}."
        "You're not carrying anything important.")))

(defn move [item coords]
 "Move an item to a location."
 (update-item item :owner None :coords coords))

;;; -----------------------------------------------------------------------------
;;; Item - Character interaction
;;; -----------------------------------------------------------------------------

(defn claim [item owner]
  "Set the owner of the item and remove its coords.
This implements picking it up, giving, taking etc."
  (update-item item :owner owner.name :coords None))

(defn drop [item owner]
  "Unset the owner and place the item at their location."
  (when (= owner.name item.owner)
    (move item owner.coords)))

(defn fuzzy-claim [obj character]
  "Check `obj` is there, then own it and assign it to character's inventory."
  (if (>= (len (inventory character)) 6)
      (choice full-inventory-messages)
      (let [items-here (get-items character.coords)
            items-here-dict (dfor i items-here i.name i)]
        (if (fuzzy-in obj (.keys items-here-dict))
            (let [item-name (best-of (.keys items-here-dict) obj)
                  i (get items-here-dict item-name)]
              (claim i character)
              f"You picked up the {i.name}.")
            f"I can't see a '{obj}' here. Are you sure that that's the right name, that it's here and that you can pick it up?"))))

(defn fuzzy-drop [obj character]
  "Check `obj` is there, then own it and assign it to character's inventory."
  (let [inv (inventory character)
        inv-names (lfor i inv i.name)]
    (if (fuzzy-in obj inv-names)
        (let [item-name (best-of inv-names obj)
              i (get-item item-name)]
          (drop i character)
          f"You dropped the {i.name}.")
        f"You don't have a '{obj}' here. Are you sure that's the right name?")))
