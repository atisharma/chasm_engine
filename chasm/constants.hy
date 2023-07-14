(import chasm.types [Character Coords])


(setv alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      alphanumeric "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")

(setv place-types ["a small building"
                   "a large building"
                   "a small outdoor space"
                   "a large outdoor space"
                   "an underground space"
                   "a space high up"]
      place-attributes ["name" "appearance" "atmosphere" "terrain"])

(setv item-attributes ["name" "appearance" "type" "usage" "item"])

(setv default-character (Character
                          :name None
                          :appearance None
                          :gender None
                          :backstory None
                          :voice None
                          :traits None
                          :motivation None
                          :likes None
                          :dislikes None
                          :skills None
                          :occupation None
                          :coords (Coords 0 0)
                          :destination "stay here"
                          :objectives None
                          :score 0
                          :health 100
                          :emotions None
                          :npc True))

(setv compass-directions ["n" "north"
                          "ne" "northeast"
                          "e" "east"
                          "se" "southeast"
                          "s" "south"
                          "sw" "southwest"
                          "w" "west"
                          "nw" "northwest"])

; target number of items / characters per place
(setv item-density 0.25
      character-density 0.25)

(setv full-inventory-messages
      ["You've got enough to carry already."
       "Your inventory is full."
       "You've got too many items already."
       "Try dropping something first."])

(setv inventory-capacity 4)
