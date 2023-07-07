"
Functions that deal with items.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import chasm.stdlib *)
(import chasm.constants [alphanumeric full-inventory-messages])
(import chasm [place])
(import chasm.types [Item Coords at?])
(import chasm.state [news world get-item set-item update-item items])
(import chasm.chat [respond
                    system user assistant])


;; TODO: modify / damage / destroy items

(defn gen [place]
  "Make up some fantastical item."
  (let [seed (choice alphanumeric)
        template f"name: item name (has '{seed}' in the first few letters)
type: item type
appearance: item's appearance
usage: what the item does"
        setting f"Story setting: {world}"
        instruction f"Below is a story setting and a template describing an item in the story.
Complete the template for a single portable object you would expect to find in the {place.name}.
Give one attribute per line, with no commentary or other notes, just the updated template with the details.
Make up a name, type, appearance, usage.
Write a very short sentence (max 10 words) for appearance and another for usage. Be very specific."
        kvs (-> (respond
                  [(system instruction)
                   (user setting)
                   (user template)]))]
    (try
      (let [details (grep-attributes kvs ["name" "appearance" "type" "usage" "item"])
            name (:name details (:item details None))]
        (log.info f"Creating item '{name}'")
        (when name
          (.pop details "name" None)
          (.pop details "item" None)
          (Item #** (| {"type" "object"
                        "appearance" "Looks like you'd expect."
                        "usage" "Usage unknown."}
                       details)
                :name name
                :coords place.coords
                :owner None)))
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
        f"Only the following portable items are laying around, which may be taken: {items-str}."
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
        f"{character.name} is carrying the following items:\n{items-str}\n"
        "{character.name} is not carrying anything important.")))

(defn move [item coords]
 "Move an item to a location."
 (update-item item :owner None :coords coords))

;;; -----------------------------------------------------------------------------
;;; Item - Character interaction
;;; -----------------------------------------------------------------------------

(defn claim [item owner]
  "Set the owner of the item and remove its coords.
This implements picking it up, taking etc."
  (update-item item :owner owner.name :coords None))

(defn drop [item owner]
  "Unset the owner and place the item at their location."
  (when (= owner.name item.owner)
    (move item owner.coords)))

(defn fuzzy-claim [obj character]
  "Check `obj` is there, then own it and assign it to character's inventory."
  (if (>= (len (inventory character)) 4)
      (choice full-inventory-messages)
      (let [items-here (get-items character.coords)
            items-here-dict (dfor i items-here i.name i)]
        (if (fuzzy-in obj (.keys items-here-dict))
            (let [item-name (best-of (.keys items-here-dict) obj)
                  i (get items-here-dict item-name)]
              (claim i character)
              f"You picked up the {i.name}.")
            f"You can't pick up '{obj}'."))))

(defn fuzzy-drop [obj character]
  "Check `obj` is there, then own it and assign it to character's inventory."
  (let [inv (inventory character)
        inv-names (lfor i inv i.name)]
    (if (fuzzy-in obj inv-names)
        (let [item-name (best-of inv-names obj)
              i (get-item item-name)]
          (drop i character)
          f"You dropped the {i.name}.")
        f"You don't have '{obj}' in your inventory.")))

(defn fuzzy-give [obj owner character]
  "Check `obj` is owned, then assign it to character's inventory."
  (let [inv (inventory owner)
        inv-names (lfor i inv i.name)]
    (if (fuzzy-in obj inv-names)
      (let [item-name (best-of inv-names obj)
            i (get inv item-name)]
        (claim i character)
        f"You gave the {i.name} to {character.name}.")
      f"You don't have a '{obj}'.")))
