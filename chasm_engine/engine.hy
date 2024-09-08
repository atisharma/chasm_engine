"
The game engine. Handles interaction between Place, Item, Character, Event and narrative.
The engine logic is expected to handle many players.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import time [time])

(import chasm_engine [log])

(import chasm_engine.stdlib *)
(import chasm_engine [place item character plot])
(import chasm_engine.types [Coords])
(import chasm_engine.constants [character-density item-density compass-directions])
(import chasm_engine.state [world world-name
                            characters
                            len-items
                            get-place len-places
                            random-coords
                            get-character update-character len-characters
                            get-account update-account get-accounts
                            get-narrative set-narrative])
(import chasm_engine.chat [OpenAIError APIError APIConnectionError ChatError respond
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

;; API functions
;; -----------------------------------------------------------------------------

(defn :async payload [narrative result player-name]
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
               "compass" (await (print-map player.coords :compass True))
               "inventory" (lfor i (item.inventory player) i.name)
               "place" (place.name player.coords)}
     "world" world-name
     "coords" player.coords}))

(defn null [#* args #** kwargs] ; -> response
  "Server no-op."
  {"error" "No valid engine function specified."})

(defn status [#* args #** kwargs] ; -> response
  "Server status."
  {"result" {"up" True}}) ; TODO: show busy (and with what), or ready for input

(defn motd [#* args #** kwargs] ; -> response
  "Server MOTD."
  {"result"
   (info
     (or (config "motd")
         (slurp (or (+ (os.path.dirname __file__) "/motd.md")
                    "chasm/motd.md"))))})

(defn :async spawn-player [player-name #* args #** kwargs] ; -> response
  "Start the game. Make sure there's a recent message. Return the whole visible state."
  (try
    (await (place.extend-map (Coords 0 0)))
    (let [coords (random-coords)
          player (await (character.spawn :name player-name :loaded kwargs :coords coords)) 
          narrative (or (get-narrative player-name)
                        (set-narrative [(user f"****") (assistant (await (describe-place player)))] player-name))]
      (update-character player :npc False)
      (await (place.extend-map coords))
      (await (payload narrative (last narrative) player.name)))
    (except [err [Exception]]
      (log.error "unknown exception" :exception err)
      (error f"Engine error: {(repr err)}"))))

(defn help-str []
  "Return the help string."
  (slurp (or (+ (os.path.dirname __file__) "/help.md")
             "chasm/help.md")))

(defn online [[long False] [seconds 600]]
  "List of player-characters online since (600) seconds ago."
  (let [chars-online (lfor a (get-accounts) :if (< (- (time) (float (:last-verified a))) seconds) (:name a))]
    (if long
        (if chars-online
            (+ (.join ", " chars-online) ".")
            "Nobody online.")
        chars-online)))

(defn :async parse [player-name line #* args #** kwargs] ; -> response
  "Process the player's input and return the whole visible state."
  (log.info f"{player-name}: {line}")
  (let [_player (or (get-character player-name) (await (character.spawn :name player-name :loaded kwargs)))
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
                   (give? line) (info (item.fuzzy-give player #* (give? line)))
                   (.startswith line "/help") (info (help-str))
                   (.startswith line "/hint") (info (await (hint messages player line)))
                   (.startswith line "/hist") (msg "history" "The story so far...")
                   (.startswith line "/map") (info (await (print-map player.coords)))
                   (.startswith line "/exits") (msg (await (print-map player.coords)))
                   (.startswith line "/online") (info (online :long True))
                   ;(.startswith line "/characters") (info (or (character.describe-at player.coords :exclude player.name) "Nobody interesting here but you.")) ; for debugging
                   ;(.startswith line "/items") (info (item.describe-at player.coords)) ; for debugging
                   ;(.startswith line "/what-if") (info (await (narrate (append (user (last (.partition line))) messages) player))) ; for debugging
                   ;; responses as assistant
                   (look? line) (assistant (await (place.describe player :messages messages :length "short")))
                   (go? line) (assistant (await (move (append user-msg messages) player)))
                   ;(talk? line) (assistant (converse (append user-msg messages) player)) ; this one needs thinking about
                   (command? line) (let [u-msg (user (get line (slice 1 None)))]
                                     (assistant (await (narrate (append u-msg messages) player))))
                   line (assistant (await (narrate (append user-msg messages) player))))
                 (except [err [APIError]]
                   (log.error "Server API error (APIError).")
                   (error "Server error: call to language model failed."))
                 (except [err [APIConnectionError]]
                   (log.error "model API connection error (APIConnectionError).")
                   (error "Server error: call to language model failed."))
                 (except [err [OpenAIError]]
                   (log.error "model API error (OpenAIError).")
                   (error "Server error: call to language model server failed."))
                 (except [err [ChatError]]
                   (log.error "Empty reply" :exception err)
                   (info "There was no reply."))
                 (except [err [Exception]]
                   (log.error "Engine error" :exception err)
                   (error f"Engine error: {(repr err)}")))]
    ; info, error do not extend narrative.
    (when (and result (= (:role result) "assistant"))
      (.extend narrative [user-msg result])
      (set-narrative (cut narrative -100 None) player-name) ; keep just last 100 messages
      (await (move-characters narrative)))
    (log.debug f"-> {result}")
    ; always return the most recent state
    (await (payload narrative result player-name))))
      
;; World functions (background tasks)
;; -----------------------------------------------------------------------------

(defn :async init []
  "When first starting the engine, create a few places to go."
  (for [x (range -4 5)
        y (range -4 5)]
    (await (place.extend-map (Coords x y)))))

(defn :async extend-world [] ; -> place or None
  "Make sure the map covers all characters. Add items, new characters if necessary.
This function does not use vdb memory so should be thread-safe."
  (for [n (.keys characters)]
    (let [c (get-character n)
          coords c.coords]
      (unless c.npc
        (await (place.extend-map coords))))))

(defn :async spawn-items [] ; -> item or None
  "Spawn items when needed at existing places."
  (let [coords (random-coords)]
    (when (> (* item-density (len-places))
             (len-items))
      (log.info f"New item at {coords}")
      (await (item.spawn coords)))))
  
(defn :async spawn-characters [] ; -> char or None
  "Spawn characters when needed at existing places."
  (let [coords (random-coords)]
    (unless (character.get-at coords)
      (when (> (* character-density (len-places))
               (len-characters))
        (log.info f"New character at {coords}")
        (await (character.spawn :name None :coords coords))))))
  
(defn :async develop [] ; -> char or None
  "Move the plot and characters along.
Writes to vdb memory so is not thread-safe."
  (when develop-queue
    (log.info f"queue: {develop-queue}")
    (let [player-name (.pop develop-queue)
          player (get-character player-name)
          messages (get-narrative player-name)
          recent-messages (cut messages -2 None) ; just new messages
          characters-here (character.get-at player.coords)]
      (when (and player-name player messages)
        ; new plot point, record in vdb, db and recent events
        (await (plot.extract-point recent-messages player))
        ; all players get developed
        (for [c characters-here]
          (await (character.develop-lines c recent-messages)))
        ; Summon npcs to the player's location if < 4 here
        (when (< (len characters-here) 4)
          (for [c-name (await (character.get-new recent-messages player))]
            (let [c (get-character c-name)]
              (if (and c c.npc)
                (character.move c player.coords) ; make sure they're here if the narrator says so
                (await (character.spawn :name c-name :coords player.coords)))))))))) ; new characters may randomly spawn if mentioned

(defn set-offline-players []
  "Set characters not accessed in last hour to NPC."
  ; TODO: maybe this is server logic, not engine?
  (for [a (get-accounts)]
    (let [dt (- (time) (:last-accessed a Inf))]
      (when (> (abs dt) 3600)
        (update-character (get-character (:name a)) :npc True)))))

;; Parser functions -> bool
;; -----------------------------------------------------------------------------

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
    (cond (= cmd "/go") (re.sub "^to " "" (sstrip dirn))
          (= cmd "go") (re.sub "^to " "" (sstrip dirn))
          (and (command? cmd) (in (rest cmd) compass-directions)) (rest (sstrip cmd)) ; '/sw' etc
          (in cmd compass-directions) (sstrip cmd)))) ; plain 'east' etc

(defn take? [line] ; -> obj or None
  "Are you trying to pick up an item?"
  (let [[_cmd _ obj] (.partition line " ")
        cmd (.lower _cmd)]
    (when (.startswith cmd "/take") (sstrip obj))))

(defn drop? [line] ; -> item or None
  "Are you trying to drop an item?"
  (let [[_cmd _ obj] (.partition line " ")
        cmd (.lower _cmd)]
    (when (.startswith cmd "/drop") (sstrip obj))))

(defn give? [line] ; -> [item character] or None
  "Are you trying to give an item?"
  (let [[cmd _ obj-recip] (.partition line " ")
        cmd (.lower cmd)
        [obj _ recipient] (.partition obj-recip " to ")]
    (when (.startswith cmd "/give") [(sstrip obj) (get-character (sstrip recipient))])))

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

;; -----------------------------------------------------------------------------

(defn talk-status [dialogue character]
  "Show chat partner, place, tokens used."
  (status-line (.join " | "
                      [f"[italic blue]{world-name}[/italic blue]"
                       f"[italic cyan]Talking to {character.name}[/italic cyan]"
                       f"{(:x character.coords)} {(:y character.coords)}"
                       f"{(+ (token-length world) (token-length dialogue))} tkns"])))

;; functions -> msg or None (with output)
;; -----------------------------------------------------------------------------

(defn :async describe-place [char]
  "Short context-free description of a place and its occupants."
  ; don't include context so we force the narrative location to change.
  (let [description (await (place.describe char))
        chars-here (character.list-at-str char.coords :exclude char.name)]
    (.join "\n\n" [description chars-here])))

(defn :async move [messages player] ; -> msg or None
  "Move the player. Describe. `dirn` may be a compass direction like 'n' or a place name like 'Small House'"
  (let [user-msg (last messages)
        line (:content user-msg)
        dirn (go? line)
        new-coords (await (place.go dirn player.coords))
        here (get-place player.coords)]
    (log.info f"{player.name} to {dirn} {player.coords} -> {new-coords}")
    (cond
      new-coords (do (character.move player new-coords)
                     ; and make sure to pass character with updated position to place.describe
                     (await (describe-place (get-character player.name))))
      (fuzzy-in dirn here.rooms) (await (narrate messages player)) ; going to a room
      :else (choice [f"You can't go to '{dirn}'."
                     f"Is '{dirn}' where you meant?"
                     f"I'm not sure '{dirn}' is a place that you can go to."
                     f"'{dirn}' doesn't seem to be a location you can go."
                     f"'{dirn}' isn't accessible from here. Try somewhere else."]))))

(defn :async hint [messages player line]
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
{(await (place.nearby-str player.coords))}
{(place.rooms player.coords)}

{characters-here}

{inventory-str}

Now give the hint."
        instruction (+ "Give a single, one-sentence hint to progress the plot and help the player achieve their objective. "
                       (if line
                           f"The hint must relate to the following question: {line}"
                           "The hint should be a riddle, maybe cryptic."))]
    (-> [(system story-guidance)
         #* messages
         (user (.join "\n\n"
                     [instruction
                      local-guidance]))]
        (truncate)
        (respond)
        (await)
        (trim-prose))))

(defn :async narrate [messages player]
  "Narrate the story, in the fictional universe."
  (let [topic (await (text->topic (format-msgs (msgs->dlg player.name "narrator" (cut messages -6 None)))))
        news (.join "\n" [(plot.news) topic]) ; smallest embedding model only takes 256 tokens
        items-here-str (item.describe-at player.coords)
        here (get-place player.coords)
        ; this includes the player
        characters-here (character.get-at player.coords)
        character-names-here (lfor c characters-here c.name)
        inventory-str (.join "\n\n" (map item.describe-inventory characters-here))
        ; if any are explicitly mentioned, give all characters a long description
        ; this gets long, so limit detailed characters to a few.
        detailed-chars (and (< (len character-names-here) 3)
                            (any (map (fn [s] (fuzzy-substr s (:content (last messages))))
                                      character-names-here)))
        character-descs-here (character.describe-at player.coords :long detailed-chars)
        present-str (if (> (len character-names-here) 1)
                        (+ (.join ", " character-names-here) f" are here at the {here.name}.")
                        f"{player.name} is at the {here.name}.")
        plot-points (.join "\n" (plot.recall-points news))
        memories (.join "\n\n" (lfor c (character.get-at player.coords)
                                     (let [mem (bullet (character.recall c (.join "\n" [c.objective
                                                                                        plot-points
                                                                                        #* character-names-here
                                                                                        news])
                                                                           :n 6))]
                                       (if mem
                                           f"{c.name} recalls the memories:\n{mem}."
                                           ""))))
        story-guidance f"You are the narrator in an immersive, enjoyable, award-winning adventure / interactive fiction game.
The player ({player.name}, or User) interjects with questions or instructions/commands, to be interpreted as instructions for the player's avatar (also {player.name}) in the context of the story.
Commands are meant for {player.name} and may be in first person ('I stand up') or imperative ('stand up', 'Look at X' or 'Ask Y about Z').
Questions are meant in the context of the story ('What am I wearing?' really means 'Narrator, describe what {player.name} is wearing' etc).
In the narrative, refer to {player.name} in the second person ('you do..., you go to...'), but an in-game character speaking directly to them may address them directly as '{player.name}'.
Indicate acting directions or actions like this: *smiles* or *shakes head*. Never break the 'fourth wall'.
Be descriptive but brief, don't give instructions, don't break character. Don't describe emotions or internal state, rather, let the player infer it from events.
If the player's last instruction is highly inconsistent in context of the story (for example 'turn into a banana' when that's impossible), just refuse to do it.
Make every effort to keep the story consistent. Any puzzles and events should develop the narrative arc. Don't allow the player to dictate the effects of their action: the narrator is in charge.
Don't describe yourself as an AI, chatbot or similar; if you can't do something, describe {player.name} doing it within the story. Don't apologize. Don't summarize. If you must refer to yourself, do so as the narrator.

Story setting:
{world}
Important plot points:
{plot-points}
An extract of the narrative is below."
        local-guidance f"Information useful to continue the narrative:
{present-str}
These places are accessible:
{(await (place.nearby-str player.coords))}
{(place.rooms player.coords)}

{items-here-str}
{character-descs-here}
{memories}

{inventory-str}

Keep {player.name} at {here.name}.
Continue the narrative, being brief."]
    (log.info f"{player.name} {player.coords}")
    ;(log.info f"{characters-here}\n{items-here-str}")
    (log.info f"news:\n{(plot.news)}")
    (log.info f"memories:\n{memories}")
    (log.info f"story: {(token-length story-guidance)}; local {(token-length local-guidance)}")
    (log.info f"inventory-str\n{inventory-str}")
    (.add develop-queue player.name) ; add the player to the development queue
    (-> (-> [(system (.join "\n\n" [story-guidance local-guidance]))
             #* messages]
            (truncate))
        (respond)
        (await)
        (trim-prose)
        (or ""))))

; FIXME: not written
(defn consume-item [messages player item]
  "The character changes the narrative and the item based on the usage.
  Rewrite the item description based on its usage.")
  ; How is it being used?
  ; What happens to the item?

; FIXME: This is not written for the current client-server paradigm

; FIXME: makes no sense in a server context

#_(defn :async converse [messages player]
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
              (let [response (await (respond [(system story-guidance)
                                              #* dialogue
                                              (user line)]))
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
                (await (character.develop-lines (get-character talk-to) (msgs->dlg player.name talk-to dlg)))
                (await (character.develop-lines player (msgs->dlg player.name talk-to dlg)))
                (await (assistant (text->topic (format-msgs (msgs->dlg player.name talk-to dlg))))))))
          (assistant (if talk-to
                         f"Why don't you talk to {talk-to} instead?"
                         f"There's nobody available with the name {talk-to-guess}.")))))

(defn :async move-characters [messages]
  "Move characters to their targets."
  (for [c (map get-character characters)]
    (when c.npc ; don't randomly move a player, only NPCs
      ; don't test for accessibility
      (let [ps (await (place.nearby c.coords :place True :list-inaccessible True))
            pnames (lfor p ps p.name)
            pname (fuzzy-in c.destination pnames)]
        (when (and pname
                   (dice 16)
                   ; don't move them if they've been mentioned in the last move or two
                   (not (in c.name (str (cut messages -4 None)))))
          (let [p (first (lfor p ps :if (= p.name pname) p))]
            (log.info f"{c.name} -> {p.name}")
            (character.move c p.coords)))))))

;; Main engine loop
;; -----------------------------------------------------------------------------

(defn :async print-map [coords [compass False]]
  "Get your bearings."
  (if compass
      (let [cx (:x coords)
            cy (:y coords)
            accessible-places (await (place.accessible coords :min-places 4))]
        (.join "\n"
          (lfor dy [1 0 -1]
                (.join ""
                       (lfor dx [-1 0 1]
                         :setv nearby-place (place.get-offset-place coords dx dy)
                         (cond (in nearby-place accessible-places) "• "
                               (= 0 (+ (abs dx) (abs dy))) "+ "
                               :else "  "))))))
      (.join "\n\n"
          [f"***{(place.name coords)}***"
           f"{(.join ", " (place.rooms coords :as-string False))}"
           f"*{(await (place.nearby-str coords))}*"])))

(defn spy [char-name]
  (-> (get-character char-name)
      (._asdict) 
      (json.dumps :indent 4)))
