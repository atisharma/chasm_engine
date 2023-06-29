(import chasm.types [Character Coords])


(setv alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      alphanumeric "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")

(setv appearances ["hairstyle" "middle-aged" "young" "old" "tall" "short" "medium height" "average height"])

(setv default-character (Character
                          :name None
                          :appearance "looks like a generic NPC"
                          :backstory "Unknown"
                          :voice "silent"
                          :traits "Unknown"
                          :motivation "Unknown"
                          :dislikes "Unknown"
                          :coords (Coords 0 0)
                          :quest None
                          :score 0
                          :memories []
                          :health 100
                          :skills None
                          :emotions None))

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
