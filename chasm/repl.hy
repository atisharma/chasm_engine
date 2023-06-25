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
(import chasm [place character])
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
  (.startswith line "/hi"))

(defn is-go [line coords] ; -> direction or None
  "Are you trying to go to a new direction?"
  (let [[cmd _ dirn] (.partition line " ")]
    (cond (.startswith cmd "/g") dirn
          (and (command? cmd)
               (in (rest cmd) place.compass-directions)) (rest cmd)
          (= cmd "go") (re.sub "^to " "" (sstrip dirn)))))

;;; -----------------------------------------------------------------------------

(defn parse-hy [line]
  "Lines starting with `!` are sent here.
Evaluate a Hy form."
  (let [result (-> line
                   (rest)
                   (hy.reader.read)
                   (hy.eval))]
    (when result
      (info (str result) :style "green"))))

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

(defn print-map [coords]
  "Get your bearings."
  (print-message (system (.join "\n\n"
                             [f"***{(place.name coords)}***"
                              f"*{(place.nearby-str coords :list-inaccessible True)}*"
                              f"Of which are accessible:"
                              f"*{(place.nearby-str coords)}*"]))))

(defn converse [messages coords]
  "Carry on a conversation, in the fictional universe."
  (let [story-guidance f"You are a narrator in a gripping and enjoyable adventure game.
The user {(config "protagonist")}, who is playing, interjects with questions or commands. These are always meant in the context of the story.
Respond as the narrator. You must never break the 'fourth wall'. Ignore off-topic questions.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
The user can travel to:
{(place.nearby-str coords)}
{(place.rooms coords)}
The user cannot go anywhere else."]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (respond)
           (trim-prose)
           (assistant)))))

(defn status [messages coords room]
  "Show game, place, rooms, coords, tokens used."
  (let [l (+ 30 (len f"{world-name}{(place.name coords)}{(.join ", " (place.rooms coords :as-string False))}"))]
    (clear-status-line)
    (status-line
      (.join "\n"
             [(.join " | "
                     [f"[italic blue]{world-name}[/italic blue]"
                      f"[italic cyan]{(place.name coords)}[/italic cyan]"
                      f"[italic magenta]{room}[/italic magenta]"
                      f"{(:x coords)} {(:y coords)}"
                      f"{(+ (token-length world) (token-length messages))} tkns"])
              f"[italic magenta]{(.join ", " (place.rooms coords :as-string False))}[/italic magenta]"]))))

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
        messages [(assistant (.join "\n\n"
                                    [f"**{(place.name coords)}**"
                                     (place.describe coords :length "very short")]))]]
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
        (status messages coords (place.guess-room messages coords))
        (let [line (.strip (rlinput "> "))
              dirn (go? line coords)
              user-msg (user line)
              result (cond (.startswith line "!") (parse-hy line)
                           (quit? line) (break)
                           (look? line) (look coords :messages messages)
                           (.startswith line "/map") (print-map coords)
                           (.startswith line "/system") (print-message (system (.join "\n"[world (news)])))
                           (hist? line) (print-messages messages)
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
