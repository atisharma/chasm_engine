"
The game engine. Handles interaction between Place, Item, Character and Dialogue.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import chasm.stdlib *)
(import chasm [place item character state])
(import chasm.constants [character-density item-density compass-directions])
(import chasm.state [world world-name news
                     username
                     get-place
                     update-character])
(import chasm.chat [respond
                    append prepend
                    user assistant system])
(import chasm.interface [spinner info error])


;;; -----------------------------------------------------------------------------
;;; Parser functions -> bool
;;; -----------------------------------------------------------------------------

(defn command? [line]
  (.startswith line "/"))

(defn quit? [line]
  (or (.startswith line "/q")
      (.startswith line "/exit")))

(defn look? [line]
  (.startswith line "/l"))

(defn hist? [line]
  (.startswith line "/hist"))

(defn go? [line] ; -> direction or None
  "Are you trying to go to a new direction?"
  (let [[cmd _ dirn] (.partition line " ")]
    (cond (.startswith (.lower cmd) "/g") dirn
          (and (command? cmd)
               (in (rest cmd) compass-directions)) (rest cmd)
          (= (.lower cmd) "go") (re.sub "^to " "" (sstrip dirn)))))

(defn take? [line] ; -> obj or None
  "Are you trying to pick up an item?"
  (let [[cmd _ obj] (.partition line " ")]
    (cond (.startswith cmd "/take") (sstrip obj)
          (= cmd "take") (sstrip obj)
          (= cmd "pick") (re.sub "^up " "" (sstrip obj)))))

(defn drop? [line] ; -> item or None
  "Are you trying to drop an item?"
  (let [[cmd _ obj] (.partition line " ")]
    (cond (.startswith cmd "/drop") (sstrip obj)
          (= cmd "drop") (sstrip obj)
          (and (= cmd "put")
               (.startswith obj "down")) (re.sub "^down " "" (sstrip obj)))))

;;; -----------------------------------------------------------------------------
;;; functions -> msg or None (with output)
;;; -----------------------------------------------------------------------------

;;; TODO: maybe abstract out a standard narrator prompt template.

(defn move [messages player line] ; -> msg-pair or None
  "Extend the map. Spawn items and characters. Move the player. Describe.
`dirn` may be a compass direction like 'n' or a place name like 'Small House'"
  (let [dirn (go? line)
        new-coords (place.go dirn player.coords)
        here (get-place player.coords)
        user-msg (user line)]
    (log.info f"engine/move: {player.name} to {dirn} {player.coords} -> {new-coords}")
    (cond
      new-coords (do
                   (with [s (spinner "Travelling...")]
                       (place.extend-map new-coords)
                       ; adding item, character has to occur *after* place has been created
                       (when (< (/ (len state.items) (inc (len state.places))) item-density))
                       (item.spawn new-coords)
                       (when (< (/ (len state.characters) (inc (len state.places))) character-density))
                       (character.spawn :name None :coords new-coords)
                       (character.move player new-coords)
                       (assistant (place.describe player
                                               :messages messages
                                               :length "very short"))))
      (fuzzy-in dirn here.rooms) (narrate (append user-msg messages) player) ; go to a room
      :else (assistant (choice [f"You can't go to '{dirn}'."
                                f"Is '{dirn}' where you meant?"
                                f"I'm not sure '{dirn}' is a place."
                                f"'{dirn}' doesn't seem to be somewhere you can go."
                                f"'{dirn}' isn't accessible from here. Try somewhere else."])))))

(defn look [player [messages None] [length "short"]]
  "Describe the immediate surroundings."
  (with [s (spinner "Writing...")]
    (assistant (place.describe player :messages messages :length length))))

(defn assess [messages coords line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.describe-at coords)
        characters-here (character.describe-at coords)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user (whose name is {username}), who is playing, interjects with questions, instructions or commands. These are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}
The user {username} cannot and will not go anywhere else, and does not want to go anywhere unless the narrator has been instructed.

{items-here}
{characters-here}

Ignore off-topic questions. Refuse instructions that are implausible or inconsistent with the lore. Do not give instructions, just be descriptive."
        instruction "Assess how plausible or consistent the instruction below is for the player to do in the context of the story. Give a score from 1 (least) to 10 (most) then explain your reasons."]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (append (user instruction))
           (append (user line))
           (respond)
           (trim-prose)
           (info)))))

(defn hint [messages coords line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.describe-at coords)
        characters-here (character.describe-at coords :long True)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user, whose name is {username} and who is playing, interjects with questions, instructions or commands.
The assistant responds in the narrator's voice.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}

{items-here}
{characters-here}"
        instruction (+ "Give a single, one-sentence hint to progress the plot. "
                       (if line
                           f"The hint must relate to the following question: {line}"
                           "The hint should be a riddle."))]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (append (user instruction))
           (respond)
           (trim-prose)
           (+ "**Hint**: ")
           (info)))))

(defn what-if [messages coords line]
  (let [[cmd _ instruction] (.partition line " ")
        items-here (item.describe-at coords)
        characters-here (character.describe-at coords :long True)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user, whose name is {username} and who is playing, interjects with questions, instructions or commands.
The assistant responds in the narrator's voice.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}

{items-here}
{characters-here}"]
    (with [s (spinner "Postulating...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (append (user instruction))
           (respond)
           (trim-prose)
           (info)))))

(defn narrate [messages player]
  "Narrate the story, in the fictional universe."
  (let [items-here-str (item.describe-at player.coords)
        inventory-str (item.describe-inventory player)
        character-names-here (lfor c (character.get-at player.coords) c.name)
        detailed-chars (any (map (fn [s] (fuzzy-substr s (:content (last messages))))
                                 character-names-here))
        ; if they're mentioned, give characters a long description
        characters-here (character.describe-at player.coords :long detailed-chars)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The player interjects with questions, instructions or commands. These commands are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.
The player is referred to in the second person by the assistant ('you do..., you go to...') and is sometimes called 'user' or '{player.name}' - these refer to the same person. Only ever refer to them as {player.name} or 'you'.

Story setting: {world}"
        local-guidance f"{(news)}
The player is still at the {(place.name player.coords)}. Do not let them go anywhere else - it would break the story.

These places are accessible:
{(place.nearby-str player.coords)}
{(place.rooms player.coords)}

{items-here-str}
{characters-here}
{inventory-str}

Assess how plausible, on-topic or consistent the instruction below is in context of the storyline. If the instruction is highly inconsistent (for example 'turn into a banana' when that's impossible), just say 'You can't do that' or some humorous variation. Make every effort to keep the story consistent.
Otherwise, follow the instruction.
The narrator does not give instructions, just be descriptive."]
    (log.info f"engine/narrate: {player.name} {player.coords} {inventory-str} {detailed-chars}")
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (respond)
           (trim-prose)
           (assistant)))))

(defn use-item [messages player item]
  "The character changes the narrative and the item based on the usage."
  "Rewrite the item description based on its usage.")
  ; How is it being used?
  ; What happens to the item?
