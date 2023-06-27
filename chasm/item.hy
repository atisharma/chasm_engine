"
Functions that deal with items.

An Item describes:
- name
- type
- appearance
- usage
- owner - None if no character has in their inventory, or character name
- coords - None if it's being carried
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import string [capwords])

(import chasm.stdlib *)
(import chasm.constants [alphanumeric])
(import chasm [place])
(import chasm.types [Item Coords at?])
(import chasm.state [news world get-item set-item update-item items])
(import chasm.chat [edit respond])


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
Write a very short sentence for appearance and another for usage. Be very specific."))
        details (grep-attributes kvs ["name" "appearance" "type" "usage"])]
    (try
      (Item #** (| {"usage" "Usage unknown." "type" "generic object" "appearance" "generic"} details)
            :coords place.coords
            :owner None)
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (log.error e)
        (log.error "Bad new item:")
        (log.error place)
        (log.error seed)
        (log.error kvs)))))

(defn spawn [coords]
  "Invent a new item from a place name, store it and return it.
None if the place doesn't exist."
  (let [p (place.get-place coords)]
    (when p
      (let [item (gen p)]
        (when (and item (not (in item.name items)))
          (set-item item)
          item)))))

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
  "List of items with a specific owner.
Usually better to use `Character.inventory`."
  (lfor item (map get-item items)
        :if (= item.owner owner.name)
        item))

(defn carried-by? [item character]
  "Is an item carried by a specific owner?"
  (= item.owner character.name))

(defn move [item coords]
 "Move an item to a location."
 (update-item item :owner None :coords coords))

(defn claim [item owner]
  "Set the owner of the item and remove its coords.
This implements picking it up, giving, taking etc."
  (update-item item :owner owner.name :coords None))

(defn drop [item owner]
  "Unset the owner and place the item at their location."
  (when (= owner.name item.owner)
    (move item owner.coords)))
