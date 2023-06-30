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
                     get-character
                     update-character])
(import chasm.chat [respond
                    truncate append prepend
                    token-length
                    user assistant system
                    msg->dlg msgs->dlg])
(import chasm.interface [spinner
                         info error
                         clear-status-line
                         print-message
                         clear-status-line
                         status-line])

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

; FIXME: have to parse this... it's complicated
; might need a LLM...
(defn give? [line] ; -> item or None
  "Are you trying to give an item?"
  (let [[cmd _ obj] (.partition line " ")]
    (cond (.startswith cmd "/give") (sstrip obj)
          (= cmd "give") (sstrip obj)
          (and (= cmd "put")
               (.startswith obj "to")) (re.sub "^to " "" (sstrip obj)))))

(defn talk? [line] ; -> string or None
  "Are you trying to talk to another character?"
  (let [[_cmd _ char] (.partition line " ")
        cmd (.lower _cmd)]
    (cond (.startswith cmd "/talk") (sstrip obj)
          (= cmd "talk") (re.sub "^to " "" (sstrip char))
          (= cmd "talk") (re.sub "^with " "" (sstrip char))
          (= cmd "chat") (re.sub "^to " "" (sstrip char))
          (= cmd "chat") (re.sub "^with " "" (sstrip char))
          (= cmd "say") (re.sub "^to " "" (sstrip char))
          (= cmd "tell") (sstrip char)
          (= cmd "ask") (sstrip char))))

;;; -----------------------------------------------------------------------------

(defn talk-status [dialogue character]
  "Show chat partner, place, tokens used."
  (clear-status-line)
  (status-line (.join " | "
                      [f"[italic blue]{world-name}[/italic blue]"
                       f"[italic cyan]Talking to {character.name}[/italic cyan]"
                       f"{(:x character.coords)} {(:y character.coords)}"
                       f"{(+ (token-length world) (token-length dialogue))} tkns"])))

;;; -----------------------------------------------------------------------------
;;; functions -> msg or None (with output)
;;; -----------------------------------------------------------------------------

;;; TODO: maybe abstract out a standard narrator prompt template.

;;; TODO: maybe abstract out a standard narrator prompt template.

(defn move [messages player] ; -> msg-pair or None
  "Extend the map. Spawn items and characters. Move the player. Describe.
`dirn` may be a compass direction like 'n' or a place name like 'Small House'"
  (let [user-msg (last messages)
        line (:content user-msg)
        dirn (go? line)
        new-coords (place.go dirn player.coords)
        here (get-place player.coords)]
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
                     ; don't include context because it forces the location to change.
                     ; and make sure to pass character with updated position to place.describe
                     (assistant (place.describe (get-player player.name) :length "very short"))))
      (fuzzy-in dirn here.rooms) (narrate messages player) ; going to a room
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
        here (get-place player.coords)
        inventory-str (item.describe-inventory player)
        character-names-here (lfor c (character.get-at player.coords) c.name)
        detailed-chars (any (map (fn [s] (fuzzy-substr s (:content (last messages))))
                                 character-names-here))
        ; if they're mentioned, give characters a long description
        characters-here (character.describe-at player.coords :long detailed-chars)
        present-str (if character-names-here
                        (+ (.join ", " character-names-here) f" and {player.name} are at the {here.name}.")
                        f"{player.name} is at the {here.name}.")
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The player ({player.name}) interjects with questions, instructions or commands. These commands are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.
The player is referred to in the second person by the assistant ('you do..., you go to...') and also sometimes called 'user' or '{player.name}' - these refer to the same person. Only ever refer to them as {player.name} or 'you'.

Story setting: {world}"
        local-guidance f"{(news)}
{present-str}
Do not let them go anywhere else unless they type 'go to {{accessible place}}'.

These places are accessible:
{(place.nearby-str player.coords)}
{(place.rooms player.coords)}

{items-here-str}
{characters-here}
{inventory-str}

Assess how plausible, on-topic or consistent the instruction below is in context of the storyline. If the instruction is highly inconsistent (for example 'turn into a banana' when that's impossible), just say 'You can't do that' or some humorous variation. Make every effort to keep the story consistent.
Otherwise, follow the instruction.
The narrator does not give instructions, just be descriptive."]
    (log.info f"engine/narrate: {player.name} {player.coords}")
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

(defn converse [messages player]
  "Have a chat with a character."
  (let [user-msg (last messages)
        _line (:content user-msg)
        line (talk? _line)
        here (get-place player.coords)
        items-here-str (item.describe-at player.coords)
        character-names-here (lfor c (character.get-at player.coords)
                                   :if (not (= c.name username))
                                   c.name)
        characters-here (character.describe-at player.coords :long True)
        talk-to-guess (first (.split line))
        talk-to (best-of character-names-here talk-to-guess)]
    (if (and talk-to (similar talk-to talk-to-guess))
        (let [story-guidance f"This is a conversation between two characters, {player.name} and {talk-to}. The assistant is playing the part of {talk-to}. The user plays the part of {player.name}. If necessary, indicate acting directions like this: *smiles* or *frowns*. You both must never break the 'fourth wall'.

Story setting:
{world}
The dialogue preserves the context of the story.
{items-here-str}
{player.name} and {talk-to} are talking here at the {here.name}. Do not go anywhere else.
{(news)}"
              late-guidance f"Now, {talk-to} will give a short reply, making every effort to keep the story on-track and consistent.
Do not speak for {player.name}. Reply as if it were {talk-to} speaking, not as anybody else. To end the conversation, or if {talk-to} is too busy, just say 'Goodbye' or say nothing."
              dialogue [(user f"Hello there, {talk-to}.")
                        (assistant f"Hello {player.name}")]]
          (clear-status-line)
          (talk-status dialogue (get-character talk-to))
          (setv chat-line (.strip (rlinput f"{player.name}: " :prefill (.join " " (rest (.split line))))))
          (log.info f"{player.name} is talking to {talk-to}.")
          ; context-stuff previous chats
          (while (and (= "assistant" (:role (last dialogue)))
                      chat-line
                      (not (.startswith chat-line "/q")))
            (.append dialogue (user chat-line))
            (let [reply-msg (with [s (spinner "Replying...")]
                              (->> dialogue
                                   (prepend (system story-guidance))
                                   (append (user line))
                                   (append (system late-guidance))
                                   (respond)
                                   (trim-prose)
                                   (assistant)))]
              (unless (or (similar "goodbye" (:content reply-msg))
                          (similar "." (:content reply-msg)))
                (.append dialogue reply-msg)
                (print-message (msg->dlg player.name talk-to reply-msg) :padding #(0 3 0 0))
                (clear-status-line)
                (talk-status dialogue (get-character talk-to))
                (setv chat-line (.strip (rlinput f"{player.name}: "))))))
          (when (> (len dialogue) 3)
            (with [s (spinner "Remembering...")]
              (character.develop (get-character talk-to) (msgs->dlg player.name talk-to dialogue))))
          None)
        (assistant (if talk-to
                       f"Why don't you talk to {talk-to} instead?"
                       f"There's nobody available with the name {talk-to-guess}.")))))
    ;save dialogue
    ;develop characters
