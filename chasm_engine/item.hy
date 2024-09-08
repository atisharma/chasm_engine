"
Functions that deal with items.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm_engine [log])

(import chasm_engine.stdlib *)
(import chasm_engine.constants [alphanumeric item-attributes full-inventory-messages inventory-capacity])
(import chasm_engine [place state])
(import chasm_engine.types [Item Coords at?])
(import chasm_engine.state [world get-item set-item update-item get-item-names])
(import chasm_engine.chat [complete-json complete-lines])


(defclass ItemError [Exception])

;; TODO: modify / damage / destroy items

(defn :async gen-json [place]
  "Make up some fantastical item."
  (let [seed (choice alphanumeric)
        template f"{{
    \"name\": \"item name (has '{seed}' in the first few letters)\",
    \"type\": \"item type\",
    \"appearance\": \"item's appearance\",
    \"usage\": \"what the item does\"
}}"
        setting f"Story setting: {world}"
        instruction f"Below is a story setting and a template describing a portable item in the story.
Complete the template for a single portable object you would expect to find in the {place.name}.
Give one attribute per line, with no commentary or other notes, just the updated template with the details.
Make up a name, type, appearance, usage.
Write a very short sentence (max 10 words) for appearance and another for usage. Be very specific."
        kvs (await (complete-json
                     :template template
                     :context setting
                     :instruction instruction))]
    (when kvs
      ; sometimes the model likes to make up an "item" field instead of "name".
      (let [details (dfor [k v] (.items kvs)
                          :if (in k ["name" "appearance" "type" "usage" "item"])
                          k v)
            name (:name details (:item details None))]
        (log.info f"Creating item '{name}'")
        (when name
          (.pop details "name" None)
          (.pop details "item" None)
          (Item #** (| {"type" "object"
                        "appearance" "Looks like you'd expect."
                        "usage" "Usage unknown."}
                       details)
                :name (re.sub r"^[Th]he " "" name)
                :coords place.coords
                :owner None))))))

(defn :async gen-lines [place]
  "Make up some fantastical item."
  (let [seed (choice alphanumeric)
        template f"name: item name (has '{seed}' in the first few letters)
type: item type
appearance: item's appearance
usage: what the item does"
        setting f"Story setting: {world}"
        instruction f"Below is a story setting and a template describing a portable item in the story.
Complete the template for a single portable object you would expect to find in the {place.name}.
Give one attribute per line, with no commentary or other notes, just the updated template with the details.
Make up a name, type, appearance, usage.
Write a very short sentence (max 10 words) for appearance and another for usage. Be very specific."
        details (await (complete-lines
                         :context setting
                         :instruction instruction
                         :template template
                         :attributes item-attributes))]
    (try
      (let [_name (.pop details "name" None)
            _alt-name (.pop details "item" None)
            name (word-chars (or _name _alt-name))]
        (log.info f"Creating item '{name}'")
        (when name
          (Item #** (| {"type" "object"
                        "appearance" "Looks like you'd expect."
                        "usage" "Usage unknown."}
                       details)
                :name name
                :coords place.coords
                :owner None)))
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (log.error "Bad new item" e)
        (log.error place)
        (log.error seed)))))

(defn :async spawn [coords]
  "Invent a new item from a place name, store it and return it.
None if the place doesn't exist or if generation fails."
  (let [p (place.get-place coords)]
    (when p
      (let [item (await (gen-lines p))]
        (when (and item
                   (not (in item.name (get-item-names))))
          (set-item item))))))

(defn get-items [coords]
  "List of (unowned) items at a location"
  (lfor item (state.get-items)
        :if (at? coords item.coords)
        item)) 

(defn describe-at [coords]
  "The prosaic version of get-items."
  (let [items (get-items coords)
        items-str (.join ", " (lfor i items i.name))
        place-name (place.name coords)]
    (if items
        f"The only notable, portable items laying around at {place-name}, which may be taken, are: {items-str}."
        "There are no portable items here.")))

(defn get-desc [item-name] ; -> str
  "Name, type and appearance of the item."
  (let [i (get-item item-name)
        owner-str (if i.owner f"- owned by {i.owner}" "")]
    (if i
        f"{i.name} ({i.type}) - {i.appearance} {i.usage} {owner-str}"
        "")))

(defn unclaimed-items []
  "List of all items (globally) without an owner.
If you just want those at a location, use `get-items`."
  (lfor item (state.get-items)
        :if (not item.owner)
        item))

(defn inventory [owner]
  "List of items with a specific owner."
  (list (filter (fn [i] (= i.owner owner.name))
                (state.get-items))))

(defn describe-inventory [character]
  "The prosaic version of get-items for an inventory."
  (let [items (inventory character)
        items-str (.join "\n" (lfor i items f"- {(get-desc i.name)}"))]
    (if items-str
        f"{character.name} has the following items in their inventory:\n{items-str}"
        f"{character.name} has no inventory.")))

(defn move [item coords]
 "Move an item to a location."
 (update-item item :owner None :coords coords))

;; Item - Character interaction
;; -----------------------------------------------------------------------------

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
  (if (>= (len (inventory character)) inventory-capacity)
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

(defn fuzzy-give [owner obj recipient]
  "Check `obj` is owned, then assign it to character's inventory."
  (log.info f"{owner.name}: {obj} -> {recipient.name}")
  (let [inv (inventory owner)
        inv-names (lfor i inv i.name)]
    (if (fuzzy-in obj inv-names)
      (let [item-name (best-of inv-names obj)
            i (get-item item-name)]
        (claim i recipient)
        f"You gave the {i.name} to {recipient.name}.")
      f"You don't have a '{obj}' to give.")))
