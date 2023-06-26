"
The main REPL where we read output and issue commands.
"
(require hyrule.argmove [-> ->> as->])

(import chasm [log])

(import os)
(import datetime [datetime])
(import readline)
(import time [sleep])
(import random [choice])

(import chasm.stdlib *)
(import chasm [place item character])
(import chasm.types [Coords])
(import chasm.state [world world-name news])
(import chasm.chat [respond token-length
                    truncate append prepend
                    user assistant system])
(import chasm.interface [banner
                         clear
                         console
                         spinner
                         clear-status-line
                         status-line
                         print-message
                         print-messages
                         info
                         error
                         exception])


(defn is-command [line]
  (.startswith line "/"))

(defn is-quit [line]
  (or (.startswith line "/q")
      (.startswith line "/exit")))

(defn is-look [line]
  (.startswith line "/l"))

(defn is-hist [line]
  (.startswith line "/hist"))

(defn is-go [line] ; -> direction or None
  "Are you trying to go to a new direction?"
  (let [[cmd _ dirn] (.partition line " ")]
    (cond (.startswith cmd "/g") dirn
          (and (command? cmd)
               (in (rest cmd) place.compass-directions)) (rest cmd)
          (= cmd "go") (re.sub "^to " "" (sstrip dirn)))))

(defn is-take [line] ; -> item or None
  "Are you trying to pick up an item?"
  (let [[cmd _ obj] (.partition line " ")]
    (cond (.startswith cmd "/take") (sstrip obj)
          (= cmd "take") (sstrip obj)
          (= cmd "pick") (re.sub "^up " "" (sstrip obj)))))

(defn is-drop [line coords] ; -> item or None
  "Are you trying to drop an item?"
  (let [[cmd _ obj] (.partition line " ")]
    (cond (.startswith cmd "/drop") (sstrip obj)
          (= cmd "drop") (re.sub "^to " "" (sstrip obj))
          (= cmd "put") (re.sub "^down " "" (sstrip obj)))))

;;; -----------------------------------------------------------------------------
;;; Debugging
;;; -----------------------------------------------------------------------------

(defn print-map [coords]
  "Get your bearings."
  (info (.join "\n\n"
            [f"***{(place.name coords)}***"
             f"{(.join ", " (place.rooms coords :as-string False))}"
             f"*{(place.nearby-str coords :list-inaccessible True)}*"
             f"Of which are accessible:"
             f"*{(place.nearby-str coords)}*"])))

(defn parse-hy [line]
  "Lines starting with `!` are sent here.
Evaluate a Hy form."
  (let [result (-> line
                   (rest)
                   (hy.reader.read)
                   (hy.eval))]
    (when result
      (info (str result) :style "green"))))

;;; -----------------------------------------------------------------------------
;;; Engine - maybe move?
;;; -----------------------------------------------------------------------------
;;; TODO: abstract out a standard narrator prompt template.

(defn move [coords dirn [messages None]] ; -> msg-pair or None
  "Extend the map. Adjust the coordinates. Describe."
  ; dirn may be a compass direction like "n" or a place name like "Small House"
  (let [new-coords (place.go dirn coords)]
    (when new-coords
      (with [s (spinner "Travelling...")]
        (place.extend-map new-coords)
        {"coords" new-coords
         "msg" (assistant (place.describe new-coords
                                          :messages messages
                                          :length "very short"))}))))

(defn look [coords [messages None] [length "short"]]
  "Describe the surroundings."
  ; TODO: speak of characters and items. Look at a character.
  (with [s (spinner "Writing...")]
    (assistant (place.describe coords :messages messages :length length))))

(defn hint [messages coords line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.get-items-str coords)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user, whose name is {(config "name")} and who is playing, interjects with questions, instructions or commands.
The assistant responds in the narrator's voice.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}

{items-here}"
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

(defn assess [messages coords line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.get-items-str coords)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user (whose name is {(config "name")}), who is playing, interjects with questions, instructions or commands. These are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}
The user {(config "name")} cannot and will not go anywhere else, and does not want to go anywhere unless the narrator has been instructed.

{items-here}

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

(defn converse [messages coords]
  "Carry on a conversation, in the fictional universe."
  (let [items-here (item.get-items-str coords)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user (whose name is {(config "name")}), who plays as the protagonist, interjects with questions, instructions or commands. These are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}
The user {(config "name")} cannot and will not go anywhere else, and does not want to go anywhere unless the narrator has been instructed.

{items-here}

Assess how plausible, on-topic or consistent the instruction below is in context of the storyline. If the instruction is highly inconsistent (for example 'turn into a banana' when that's impossible), just say 'You can't do that' or some humorous variation.
Otherwise, follow the instruction.
The narrator does not give instructions, just be descriptive."]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (respond)
           (trim-prose)
           (assistant)))))

(defn status [messages coords]
  "Show game, place, coords, tokens used."
  (let [l (+ 30 (len f"{world-name}{(place.name coords)}"))]
    (clear-status-line)
    (status-line
      (.join " | "
             [f"[italic blue]{world-name}[/italic blue]"
              f"[italic cyan]{(place.name coords)}[/italic cyan]"
              f"{(:x coords)} {(:y coords)}"
              f"{(+ (token-length world) (token-length messages))} tkns"]))))

;;; -----------------------------------------------------------------------------

(defn run []
  "Launch the REPL, which takes user input, parses
it, and passes it to the appropriate action."
  (log.info f"Starting repl at {(.isoformat (datetime.today))}")
  (banner)
  ;(info "Enter **/help** for help\n")
  (console.rule)
  (let [history-file (os.path.join (os.path.expanduser "~") ".chasm_history")
        username (config "user")
        coords (with [s (spinner "Spawning...")]
                 (character.spawn username :box [-3 3]))
        messages (with [s (spinner "Writing...")]
                   [(assistant (.join "\n\n"
                                      [f"**{(place.name coords)}**"
                                       (place.describe coords :length "very short")]))])]
    (try
      (readline.set-history-length 100)
      (readline.read-history-file history-file)
      (except [e [FileNotFoundError]]))
    ;(console.print)
    ;(print-message (system f"*{world}*"))
    (print-message (last messages))
    (while True
      (try ; ----- parser block ----- 
        (setv messages (truncate messages
                                 :length (- (config "context_length")
                                            (token-length world)
                                            750))) 
        (status messages coords)
        (let [line (.strip (rlinput "> "))
              dirn (go? line)
              user-msg (user line)
              result (cond (.startswith line "!") (parse-hy line)
                           (quit? line) (break)
                           (look? line) (look coords :messages messages)
                           (hist? line) (print-messages messages)
                           (.startswith line "/items") (info (item.get-items-str coords))
                           (.startswith line "/map") (print-map coords)
                           (.startswith line "/assess") (assess messages coords line)
                           (.startswith line "/hint") (hint messages coords line)
                           ;(.startswith line "/system") (print-message (system (.join "\n"[world (news)])))
                           dirn (let [x (move coords dirn :messages messages)]
                                  (if x (do (setv coords (:coords x)) (:msg x))
                                        (converse (append user-msg messages) coords)))
                           (command? line) (error "I didn't understand that command.")
                           line (converse (append user-msg messages) coords))]
          (when result
            (print-message result)
            (.extend messages [user-msg result])))
        (except [KeyboardInterrupt]
          (print)
          (error "**/quit** to exit"))
        (except [e [Exception]]
          (with [f (open "traceback.log" :mode "w" :encoding "UTF-8")]
            (import traceback)
            (traceback.print-exception e :file f))
          (exception)
          (sleep 5))))
    (readline.write-history-file history-file)
    (clear)))
