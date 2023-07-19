"
The game engine. Handles interaction between Place, Item, Character, Event and narrative.
The engine logic is expected to handle many players.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import chasm.stdlib *)
(import chasm [place item character plot state])
(import chasm.constants [character-density item-density compass-directions])
(import chasm.state [world world-name
                     characters
                     get-place
                     random-coords
                     get-character update-character
                     get-account update-account
                     get-narrative set-narrative])
(import chasm.chat [APIConnectionError ChatError respond
                    msgs->topic text->topic msgs->points
                    msg->dlg msgs->dlg
                    truncate standard-roles
                    token-length
                    msg user assistant system])


(defclass EngineError [Exception])

(setv develop-queue (set))

(defn info [content]
  (msg "info" content))

(defn error [content]
  (msg "error" content))

;;; -----------------------------------------------------------------------------
;;; API functions
;;; -----------------------------------------------------------------------------

(defn payload [narrative result player-name]
  "What the client expects."
  (let [player (get-character player-name)
        account (get-account player-name)]
    (update-account player-name :turns (inc (:turns account 0)))
    {"narrative" narrative
     "result" result
     "player" {"name" player.name
               "objective" player.objective
               "score" player.score
               "turns" (:turns account None)
               "health" player.health
               "coords" player.coords
               "compass" (print-map player.coords :compass True)
               "inventory" (lfor i (item.inventory player) i.name)
               "place" (place.name player.coords)}
     "world" world-name
     "coords" player.coords
     "errors" None})) 

(defn null [#* args #** kwargs] ; -> response
  "Server no-op."
  {"errors" "No valid function specified."})

(defn spawn-player [player-name #* args #** kwargs] ; -> response
  "Start the game. Make sure there's a recent message. Return the whole visible state."
  (try
    (let [player (character.spawn :name player-name :loaded kwargs :coords (random-coords)) 
          narrative (or (get-narrative player-name)
                        (set-narrative [(assistant (describe-place player))] player-name))]
      (update-character player :npc False)
      (payload narrative (last narrative) player.name))
    (except [err [Exception]]
      (log.error "engine/spawn-player:" :exception err)
      (error f"Engine error: {(repr err)}"))))

(defn help-str []
  "Return the help string."
  (slurp (or (+ (os.path.dirname __file__) "/help.md")
             "chasm/help.md")))

(defn parse [player-name line #* args #** kwargs] ; -> response
  "Process the player's input and return the whole visible state."
  (log.info f"engine/parse: {player-name}: {line}")
  (let [_player (or (get-character player-name) (character.spawn :name player-name :loaded kwargs))
        player (update-character _player :npc False)
        narrative (get-narrative player-name)
        messages (truncate (standard-roles narrative)
                           :spare-length (+ (token-length world) (config "max_tokens"))) 
        user-msg (user line)
        result (try
                 (cond
                   ;; responses as info / error
                   (quit? line) (do (update-character player :npc True) (msg "QUIT" "QUIT"))
                   (take? line) (info (item.fuzzy-claim (take? line) player))
                   (drop? line) (info (item.fuzzy-drop (drop? line) player))
                   (.startswith line "/characters") (info (or (character.describe-at player.coords :exclude player.name) "Nobody interesting here but you.")) ; for debugging
                   (.startswith line "/help") (info (help-str))
                   (.startswith line "/hint") (info (hint messages player line))
                   (.startswith line "/hist") (msg "history" "The story so far...")
                   (.startswith line "/items") (info (item.describe-at player.coords)) ; for debugging
                   (.startswith line "/map") (info (print-map player.coords))
                   (.startswith line "/spy") (info (spy (last (.partition line " ")))) ; for debugging
                   (.startswith line "/what-if") (info (narrate (append (user (last (.partition line))) messages) player)) ; for debugging
                   ;; responses as assistant
                   (look? line) (assistant (place.describe player :messages messages :length "short"))
                   (command? line) (error "I didn't understand that command.")
                   (go? line) (assistant (move (append user-msg messages) player))
                   ;(talk? line) (assistant (converse (append user-msg messages) player)) ; this one needs thinking about
                   line (assistant (narrate (append user-msg messages) player)))
                 (except [err [APIConnectionError]]
                   (log.error "engine/parse: model API connection error.")
                   (error "Server error: call to language model failed."))
                 (except [err [ChatError]]
                   (log.error "engine/parse:" :exception err)
                   (info "There was no reply."))
                 (except [err [Exception]]
                   (log.error "engine/parse:" :exception err)
                   (error f"Engine error: {(repr err)}")))]
    ; info, error do not extend narrative.
    (when (and result (= (:role result) "assistant"))
      (.extend narrative [user-msg result])
      (set-narrative (cut narrative -1000 None) player-name) ; keep just last 1000 messages
      (move-characters narrative))
    (log.debug f"engine/parse: -> {result}")
    ; always return the most recent state
    (payload narrative result player-name)))
      
;;; -----------------------------------------------------------------------------
;;; World functions (background tasks)
;;; -----------------------------------------------------------------------------

(defn extend-world []
  "Make sure the map covers all characters. Add items, new characters if necessary.
This function does not use vdb memory so should be thread-safe."
  (for [n (.keys characters)]
    (let [c (get-character n)
          coords c.coords]
      (unless c.npc
        (place.extend-map coords)
        ; adding item, character has to occur *after* place has been created
        (when (and (not (item.get-items coords))
                   (< (/ (len state.items) (inc (len state.places)))
                      item-density))
          (item.spawn coords))
        ; only spawn a new character if there's nobody there
        (when (and (not (character.get-at coords))
                   (< (/ (len state.characters) (inc (len state.places)))
                      character-density))
          (character.spawn :name None :coords coords))))))

(defn develop []
  "Move the plot and characters along. Writes to vdb memory so is not thread-safe."
  (when develop-queue
    (log.info f"engine/develop: queue: {develop-queue}")
    (let [player-name (.pop develop-queue)
          player (get-character player-name)
          messages (get-narrative player-name)]
      (when (and player-name player messages)
        ; new plot point, record in vdb, db and recent events
        (plot.extract-point messages player)
        ; all players get developed
        (for [c (character.get-at player.coords)]
          (character.develop-lines c messages))
        ; Summon npcs to the player's location
        (for [c-name (character.get-new messages player)]
          (let [c (get-character c-name)]
            (if (and c c.npc)
              (character.move c player.coords) ; make sure they're here if the narrator says so
              (character.spawn :name c-name :coords player.coords)))))))) ; new characters may randomly spawn if mentioned
  
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
  (status-line (.join " | "
                      [f"[italic blue]{world-name}[/italic blue]"
                       f"[italic cyan]Talking to {character.name}[/italic cyan]"
                       f"{(:x character.coords)} {(:y character.coords)}"
                       f"{(+ (token-length world) (token-length dialogue))} tkns"])))

;;; -----------------------------------------------------------------------------
;;; functions -> msg or None (with output)
;;; -----------------------------------------------------------------------------

(defn describe-place [char]
  "Short context-free description of a place and its occupants."
  ; don't include context so we force the narrative location to change.
  (let [description (place.describe char)
        chars-here (character.list-at-str char.coords :exclude char.name)]
    (.join "\n\n" [description chars-here])))

(defn move [messages player] ; -> msg or None
  "Move the player. Describe. `dirn` may be a compass direction like 'n' or a place name like 'Small House'"
  (let [user-msg (last messages)
        line (:content user-msg)
        dirn (go? line)
        new-coords (place.go dirn player.coords)
        here (get-place player.coords)]
    (log.info f"engine/move: {player.name} to {dirn} {player.coords} -> {new-coords}")
    (cond
      new-coords (do (character.move player new-coords)
                     ; and make sure to pass character with updated position to place.describe
                     (describe-place (get-character player.name)))
      (fuzzy-in dirn here.rooms) (narrate messages player) ; going to a room
      :else (choice [f"You can't go to '{dirn}'."
                     f"Is '{dirn}' where you meant?"
                     f"I'm not sure '{dirn}' is a place that you can go to."
                     f"'{dirn}' doesn't seem to be a location you can go."
                     f"'{dirn}' isn't accessible from here. Try somewhere else."]))))

(defn hint [messages player line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.describe-at player.coords)
        characters-here (character.describe-at player.coords :long True)
        inventory-str (item.describe-inventory player)
        story-guidance f"You are a narrator in a gripping and enjoyable adventure game.
The player/protagonist, {player.name}, interjects with questions, instructions or commands.
You respond in the narrator's voice. The narrative is given below.

Story setting: {world}"
        local-guidance f"{(plot.news)}
{player.name} is now at {(place.name player.coords)}.
{player.name}'s objective: {player.objective}

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
                           "The hint should be a riddle, maybe cryptic."))]
    (->> [(system story-guidance)
          #* messages
          (system instruction)
          (user local-guidance)]
         (truncate)
         (respond)
         (trim-prose))))

(defn narrate [messages player]
  "Narrate the story, in the fictional universe."
  (let [items-here-str (item.describe-at player.coords)
        here (get-place player.coords)
        inventory-str (item.describe-inventory player)
        ; this includes the player
        character-names-here (lfor c (character.get-at player.coords) c.name)
        ; if any are explicitly mentioned, give all characters a long description
        ; this gets long, so limit detailed characters to a few.
        detailed-chars (and (< (len character-names-here) 3)
                            (any (map (fn [s] (fuzzy-substr s (:content (last messages))))
                                      character-names-here)))
        characters-here (character.describe-at player.coords :long detailed-chars)
        present-str (if (> (len character-names-here) 1)
                        (+ (.join ", " character-names-here) f" are here at the {here.name}.")
                        f"{player.name} is at the {here.name}.")
        ; TODO: improve memory queues
        plot-points (.join "\n" (plot.recall-points (plot.news)))
        memories (.join "\n\n" (lfor c (character.get-at player.coords)
                                     (let [mem (bullet (character.recall c (.join "\n" [#* characters-here
                                                                                        plot-points
                                                                                        (plot.news)])
                                                                           :n 6))]
                                       (if mem
                                           f"{c.name} recalls the memories:\n{mem}."
                                           ""))))
        story-guidance f"You are the narrator in an immersive, enjoyable, award-winning adventure / interactive fiction game.
The player ({player.name} or user) interjects with questions or instructions/commands, to be interpreted as instructions for the player's avatar (also {player.name}) in the context of the story.
Commands are meant for {player.name} and may be in first person ('I stand up') or imperative ('stand up', 'Look at X' or 'Ask Y about Z').
Questions are meant in the context of the story ('What am I wearing?' really means 'Narrator, describe what {player.name} is wearing' etc).
In the narrative, refer to {player.name} in the second person ('you do..., you go to...'), but a character speaking directly to them may address them directly as '{player.name}'.
Indicate acting directions or actions like this: *smiles* or *shakes head*. Never break the 'fourth wall'.
Be descriptive, don't give instructions, don't break character.
If the player's last instruction is highly inconsistent in context of the story (for example 'turn into a banana' when that's impossible), just say 'You can't do that' or some variation or interpret the instruction creatively.
Make every effort to keep the story consistent. Puzzles (if any) and events should develop the narrative arc.
Don't describe yourself as an AI, chatbot or similar; if you can't do something, describe {player.name} doing it within the story. If you must refer to yourself, do so as the narrator.
Story setting:
{world}
Important plot points:
{plot-points}
An extract of the narrative is below."
        local-guidance f"Information useful to continue the narrative:
{present-str}
These places are accessible:
{(place.nearby-str player.coords)}
{(place.rooms player.coords)}

{items-here-str}
{characters-here}
{memories}

{inventory-str}

Keep {player.name} at {here.name}.
Continue the narrative."]
    (log.info f"engine/narrate: {player.name} {player.coords}")
    ;(log.info f"{characters-here}\n{items-here-str}")
    (log.info f"engine/narrate: news:\n{(plot.news)}")
    (log.info f"engine/narrate: memories:\n{memories}")
    (log.info f"engine/narrate: story: {(token-length story-guidance)}; local {(token-length local-guidance)}")
    (.add develop-queue player.name) ; add the player to the development queue
    (-> [(system story-guidance)
         (system local-guidance)
         #* messages]
        (truncate)
        (respond)
        (trim-prose)
        (or ""))))

; FIXME: not written
(defn consume-item [messages player item]
  "The character changes the narrative and the item based on the usage.
  Rewrite the item description based on its usage.")
  ; How is it being used?
  ; What happens to the item?

; FIXME: This is not written for the new client-server paradigm

; FIXME: makes no sense in a server context
(defn converse [messages player]
  "Have a chat with a character."
  (let [user-msg (last messages)
        _line (:content user-msg)
        line (talk? _line)
        recent (text->topic (format-msgs (msgs->dlg player.name "narrator" (cut messages -8 None))))
        here (get-place player.coords)
        items-here-str (item.describe-at player.coords)
        character-names-here (lfor c (character.get-at player.coords :exclude player.name) c.name)
        characters-here (character.describe-at player.coords :long True)
        talk-to-guess (first (.split line))
        talk-to (best-of character-names-here talk-to-guess)]
    (if (and talk-to (similar talk-to talk-to-guess))
        (let [story-guidance f"You are playing the character of {talk-to} in an improvised, continuous dialogue between {player.name} and {talk-to}. The user plays the part of {player.name}.
{player.name} may speak about their thoughts, feelings, intentions etc. If necessary, indicate acting directions or actions like this: *smiles* or *shakes his head*. You both must never break the 'fourth wall'.

The setting for the dialogue is:
{world}
{(plot.news)}

{items-here-str}

{player.name} and {talk-to} are talking here at the {here.name}. Do not go anywhere else.

Do not say you're an AI assistant or similar. To end the conversation, just say the codewords 'END CONVERSATION'. {player.name} will speak, then give your reply playing the part of {talk-to}, keeping in-character."
              dialogue [(user f"*Greets {talk-to}*")
                        (assistant f"*Waves back at {player.name}*")]]
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
                (talk-status dialogue (get-character talk-to))
                (setv chat-line (.strip (rlinput f"{player.name}: "))))))
          (when (> (len dialogue) 5)
            (let [dlg (cut dialogue 2 None)]
              (character.develop-lines (get-character talk-to) (msgs->dlg player.name talk-to dlg))
              (character.develop-lines player (msgs->dlg player.name talk-to dlg))
              (assistant (text->topic (format-msgs (msgs->dlg player.name talk-to dlg)))))))
        (assistant (if talk-to
                       f"Why don't you talk to {talk-to} instead?"
                       f"There's nobody available with the name {talk-to-guess}.")))))

(defn move-characters [messages]
  "Move characters to their targets."
  (for [c (map get-character characters)]
    (when c.npc ; don't randomly move a player, only NPCs
      ; don't test for accessibility
      (for [p (place.nearby c.coords :place True :list-inaccessible True)]
        (when (and (similar c.destination p.name)
                   (dice 16)
                   ; don't move them if they've been mentioned in the last move or two
                   (not (in c.name (str (cut messages -4 None)))))
          (log.info f"engine/move-characters: {c.name} -> {p.name}")
          (character.move c p.coords))))))

;;; -----------------------------------------------------------------------------
;;; Main engine loop
;;; -----------------------------------------------------------------------------

(defn print-map [coords [compass False]]
  "Get your bearings."
  (if compass
      (let [cx (:x coords)
            cy (:y coords)
            accessible-places (place.accessible coords :min-places 4)]
        (.join "\n"
          (lfor dy [-1 0 1]
                (.join ""
                       (lfor dx [-1 0 1]
                         :setv nearby-place (get-offset-place coords dx dy)
                         (cond (in nearby-place accessible-places) "•"
                               (= 0 (+ (abs dx) (abs dy))) "+"
                               :else " "))))))
      (.join "\n\n"
          [f"***{(place.name coords)}***"
           f"{(.join ", " (place.rooms coords :as-string False))}"
           f"*{(place.nearby-str coords)}*"])))

(defn spy [char-name]
  (character.describe
    (get-character char-name)
    :long True))
