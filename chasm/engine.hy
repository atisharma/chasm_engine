"
The game engine. Handles interaction between Place, Item, Character, Event and narrative.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import chasm.stdlib *)
(import chasm [place item character plot state])
(import chasm.constants [character-density item-density compass-directions])
(import chasm.state [world world-name news
                     characters
                     username
                     get-place
                     get-character
                     update-character])
(import chasm.chat [respond msgs->topic text->topic
                    truncate
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
  (let [[_cmd _ dirn] (.partition line " ")
        cmd (.lower _cmd)]
    (cond (.startswith (.lower cmd) "/g") dirn
          (and (command? cmd)
               (in (rest cmd) compass-directions)) (rest cmd)
          (= cmd "go") (re.sub "^to " "" (sstrip dirn)))))

(defn take? [line] ; -> obj or None
  "Are you trying to pick up an item?"
  (let [[_cmd _ obj] (.partition line " ")
        cmd (.lower _cmd)]
    (cond (.startswith cmd "/take") (sstrip obj))))
          ;(= cmd "take") (sstrip obj)
          ;(= cmd "pick") (re.sub "^up " "" (sstrip obj)))))

(defn drop? [line] ; -> item or None
  "Are you trying to drop an item?"
  (let [[_cmd _ obj] (.partition line " ")
        cmd (.lower _cmd)]
    (cond (.startswith cmd "/drop") (sstrip obj))))
          ;(= cmd "drop") (sstrip obj))))
          ;(and (= cmd "put") (.startswith obj "down")) (re.sub "^down " "" (sstrip obj)))))

; FIXME: have to parse this... it's complicated
; might need a LLM...

(defn give? [line] ; -> item or None
  "Are you trying to give an item?"
  (let [[cmd _ obj] (.partition line " ")
        cmd (.lower _cmd)]
    (cond (.startswith cmd "/give") (sstrip obj)
          (= cmd "give") (sstrip obj))))

(defn talk? [line] ; -> string or None
  "Are you trying to talk to another character?"
  (let [[_cmd _ char] (.partition line " ")
        cmd (.lower _cmd)]
    (when (.startswith cmd "/talk") (sstrip char)
      #_(
          (= cmd "talk") (re.sub "^to " "" (sstrip char))
          (= cmd "talk") (re.sub "^with " "" (sstrip char))
          (= cmd "chat") (re.sub "^to " "" (sstrip char))
          (= cmd "chat") (re.sub "^with " "" (sstrip char))
          (= cmd "say") (re.sub "^to " "" (sstrip char))
          (= cmd "tell") (sstrip char)
          (= cmd "ask") (sstrip char)))))

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

(defn describe-place [char]
  "Short context-free description of a place and its occupants."
  ; don't include context so we force the narrative location to change.
  (let [description (place.describe char)
        chars-here (character.list-at-str char.coords)]
    (assistant (.join "\n\n" [description chars-here]))))

(defn move [messages player] ; -> msg or None
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
                     (when (and (not (item.get-items new-coords))
                                (< (/ (len state.items) (inc (len state.places)))
                                   item-density))
                       (item.spawn new-coords))
                     ; only spawn a character if there's nobody there
                     (when (and (not (character.get-at new-coords))
                                (< (/ (len state.characters)
                                      (inc (len state.places)))
                                   character-density))
                       (character.spawn :name None :coords new-coords))
                     (character.move player new-coords)
                     ; and make sure to pass character with updated position to place.describe
                     (describe-place (get-character player.name))))
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

(defn hint [messages player line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.describe-at player.coords)
        characters-here (character.describe-at player.coords :long True)
        inventory-str (item.describe-inventory player)
        story-guidance f"You are a narrator in a gripping and enjoyable adventure game.
The player, {player.name}, interjects with questions, instructions or commands.
You respond in the narrator's voice. The narrative is given below.

Story setting: {world}"
        local-guidance f"{(news)}
{player.name} is now at {(place.name player.coords)}.
{player.name}'s objective: {player.objectives}

{items-here}

These places are accessible:
{(place.nearby-str player.coords)}
{(place.rooms player.coords)}

{characters-here}

{inventory-str}

Now give the hint."
        instruction (+ "Give a single, one-sentence hint to progress the plot and help the player achieve their objective. "
                       (if line
                           f"The hint must relate to the following question: {line}"
                           "The hint should be a riddle."))]
    (with [s (spinner "Writing...")]
      (->> [(system story-guidance)
            #* messages
            (system instruction)
            (user local-guidance)]
           (truncate)
           (respond)
           (trim-prose)
           (info)))))

(defn narrate [messages player]
  "Narrate the story, in the fictional universe."
  (let [items-here-str (item.describe-at player.coords)
        here (get-place player.coords)
        inventory-str (item.describe-inventory player)
        character-names-here (lfor c (character.get-at player.coords) c.name)
        ; if any are explicitly mentioned, give all characters a long description
        ; this gets long, so limit detailed characters to 1 or 2.
        detailed-chars (and (< (len character-names-here) 2)
                            (any (map (fn [s] (fuzzy-substr s (:content (last messages))))
                                      character-names-here)))
        characters-here (character.describe-at player.coords :long detailed-chars)
        present-str (if character-names-here
                        (+ (.join ", " character-names-here) f" and {player.name} are at the {here.name}.")
                        f"{player.name} is at the {here.name}.")
        ; TODO: memory system, topic
        memories (lfor c (character.get-at player.coords)
                       (if c.memories
                           f"{c.name} has the memories: {(character.recall c :keywords player.name)}."
                           ""))
        story-guidance f"You are the narrator in an immersive and enjoyable adventure game.
The player ({player.name}, 'you' or 'I') interjects with questions, instructions or commands. These commands are always meant in the context of the story, for instance 'Look at X' or 'Ask Y about Z'.
In your narrative, the player is referred to in the second person ('you do..., you go to...') or 'user' or '{player.name}' - these refer to the same person. Only ever refer to them as {player.name} or 'you'.

Story setting: {world}"
        local-guidance f"{(news)}
{present-str}

These places are accessible:
{(place.nearby-str player.coords)}
{(place.rooms player.coords)}

{items-here-str}

{characters-here}
{memories}

{inventory-str}

If the instruction is highly inconsistent in context of the story (for example 'turn into a banana' when that's impossible), just say 'You can't do that' or some humorous variation. Make every effort to keep the story consistent. Otherwise, follow the instruction and develop the story.
Keep {player.name} at {here.name}.
Don't give instructions, be descriptive, don't break character, don't describe yourself as an AI assistant or chatbot, maintain the fourth wall."]
    (log.info f"engine/narrate: {player.name} {player.coords}")
    ;(log.info f"{characters-here}\n{items-here-str}")
    (log.info f"engine/narrate: story: {(token-length story-guidance)}; local {(token-length local-guidance)})")
    (with [s (spinner "Writing...")]
      (-> [(system story-guidance)
           #* messages
           (system local-guidance)]
          (truncate)
          (respond)
          (trim-prose)
          (assistant)))))

(defn consume-item [messages player item]
  "The character changes the narrative and the item based on the usage."
  "Rewrite the item description based on its usage.")
  ; How is it being used?
  ; What happens to the item?

(defn converse [messages player]
  "Have a chat with a character."
  (let [user-msg (last messages)
        _line (:content user-msg)
        line (talk? _line)
        recent (text->topic (format-msgs (msgs->dlg player.name "narrator" (cut messages -8 None))))
        here (get-place player.coords)
        items-here-str (item.describe-at player.coords)
        character-names-here (lfor c (character.get-at player.coords)
                                   :if (not (= c.name username))
                                   c.name)
        characters-here (character.describe-at player.coords :long True)
        talk-to-guess (first (.split line))
        talk-to (best-of character-names-here talk-to-guess)]
    (if (and talk-to (similar talk-to talk-to-guess))
        (let [story-guidance f"You are playing the character of {talk-to} in an improvised, continuous dialogue between {player.name} and {talk-to}. The user plays the part of {player.name}.
{player.name} may speak about their thoughts, feelings, intentions etc. If necessary, indicate acting directions like this: *smiles* or *frowns*. You both must never break the 'fourth wall'.

The setting for the dialogue is:
{world}
{(news)}

{items-here-str}

{player.name} and {talk-to} are talking here at the {here.name}. Do not go anywhere else.
Recent events: {recent}

Do not say you're an AI assistant. To end the conversation, just say the codewords 'END CONVERSATION'. {player.name} will speak, then give your reply playing the part of {talk-to}, keeping in-character."
              dialogue [(user f"*Greets {talk-to}*")
                        (assistant f"*Waves back at {player.name}*")]]
          (clear-status-line)
          (talk-status dialogue (get-character talk-to))
          (setv chat-line (.strip (rlinput f"{player.name}: " :prefill (.join " " (rest (.split line))))))
          (log.info f"{player.name} is talking to {talk-to}.")
          ; context-stuff previous chats
          (while (and (= "assistant" (:role (last dialogue)))
                      chat-line
                      (not (.startswith chat-line "/q")))
            (.append dialogue (user chat-line))
            (let [response (respond [(system story-guidance)
                                     #* dialogue
                                     (user line)])
                  reply-msg (-> response
                                (trim-prose)
                                (assistant))]
              (unless (or (similar "END CONVERSATION" (:content reply-msg))
                          (similar "." (:content reply-msg)))
                (.append dialogue reply-msg)
                (print-message (msg->dlg player.name talk-to reply-msg) :padding #(0 3 0 0))
                (clear-status-line)
                (talk-status dialogue (get-character talk-to))
                (setv chat-line (.strip (rlinput f"{player.name}: "))))))
          (when (> (len dialogue) 5)
            (with [s (spinner "Making memories...")
                   dlg (cut dialogue 2 None)]
              (character.develop-lines (get-character talk-to) (msgs->dlg player.name talk-to dlg))
              (character.develop-lines player (msgs->dlg player.name talk-to dlg))))
          (assistant (text->topic (format-msgs (msgs->dlg player.name talk-to dlg)))))
        (assistant (if talk-to
                       f"Why don't you talk to {talk-to} instead?"
                       f"There's nobody available with the name {talk-to-guess}.")))))

(defn develop [messages player]
  "Move the plot and characters along."
  (with [s (spinner "Developing plot...")]
    (plot.extract-point messages player))
  (with [s (spinner "Developing characters...")]
    ; all players get developed
    (for [c (append (get-character player.name) (character.get-at player.coords))]
      (character.develop-lines c messages))
    ; Summon characters to the player's location
    (for [c-name (character.get-new messages player)]
      (let [c (get-character c-name)]
        (if c
            (character.move c player.coords) ; make sure they're here if the narrator says so
            (character.spawn :name c-name :coords player.coords)))))) ; new characters may randomly spawn if mentioned

(defn move-characters [messages player]
  "Move characters to their targets."
  (with [s (spinner "Moving other characters...")]
    (for [c-key (.keys characters)]
      (let [c (get-character c-key)]
        (unless (= c.name player.name) ; don't randomly move the player
          ; don't test for accessibility
          (for [p (place.nearby c.coords :place True :list-inaccessible True)]
            (when (and (similar c.destination p.name)
                       (dice 16)
                       ; don't move them if they've been mentioned in the last move or two
                       (not (in c.name (str (cut messages -4 None)))))
              (when (= p.coords c.coords)
                (info f"{c.name} has gone to {p.name}."))
              (character.move c p.coords))))))))
