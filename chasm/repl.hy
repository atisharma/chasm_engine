"
The main REPL where we read output and issue commands.
"
(require hyrule.argmove [-> ->> as->])

(import chasm [log])

(import os)
(import datetime [datetime])
(import readline)
(import time [sleep])

(import chasm.stdlib *)
(import chasm.engine *)
(import chasm [place item character])
(import chasm.types [Coords])
(import chasm.state [world world-name news username
                     get-character])
(import chasm.chat [token-length
                    load-messages save-messages
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

(defn status [messages player]
  "Show game, place, inventory, tokens used."
  (let [l (+ 30 (len f"{world-name}{(place.name player.coords)}"))
        inventory (.join ", " (lfor i (item.inventory player) i.name))]
    (clear-status-line)
    (status-line
      (.join "\n"
             [(.join " | "
                     [f"[italic blue]{world-name}[/italic blue]"
                      f"[italic cyan]{(place.name player.coords)}[/italic cyan]"
                      f"{(:x player.coords)} {(:y player.coords)}"
                      f"{(+ (token-length world) (token-length messages))} tkns"])
              f"[italic magenta]{inventory}[italic magenta]"]))))

(defn run []
  "Launch the REPL, which takes player's input, parses
it, and passes it to the appropriate action."
  (log.info f"Starting repl at {(.isoformat (datetime.today))}")
  (banner)
  ;(info "Enter **/help** for help\n")
  (console.rule)
  (let [history-file (os.path.join (os.path.expanduser "~") ".chasm_history")
        _coords (place.random-coords #(-2 2))
        _p (with [s (spinner "Building world...")]
             (place.extend-map _coords)) ; ensure there's somwhere to go
        player (with [s (spinner "Spawning...")]
                 (character.spawn :name username :coords _coords))
        _p (with [s (spinner "Building world...")]
             (place.extend-map player.coords)) ; ensure there's somwhere to go
        messages (or (load-messages)
                     (with [s (spinner "Writing...")]
                       [(assistant (.join "\n\n"
                                          [f"**{(place.name player.coords)}**"
                                           (place.describe player :length "very short")]))]))]
    (try
      (readline.set-history-length 100)
      (readline.read-history-file history-file)
      (except [e [FileNotFoundError]]))
    (print-message (last messages))
    (while True
      (try ; ----- parser block ----- 
        (setv messages (truncate messages
                                 :length (- (config "context_length")
                                            (token-length world)
                                            750))) 
        (setv player (get-character player.name)) ; reload every loop to sync from db
        (status messages player)
        (let [line (.strip (rlinput "> "))
              user-msg (user line)
              result (cond (.startswith line "!") (parse-hy line)
                           (quit? line) (break)
                           (look? line) (look player :messages messages)
                           (take? line) (assistant (item.fuzzy-claim (take? line) player))
                           (drop? line) (assistant (item.fuzzy-drop (drop? line) player))
                           (hist? line) (print-messages messages)
                           (.startswith line "/banner") (banner)
                           (.startswith line "/clear") (clear)
                           (.startswith line "/items") (info (item.describe-at player.coords))
                           (.startswith line "/characters") (info (or (character.describe-at player.coords) "Nobody interesting here but you."))
                           (.startswith line "/map") (print-map player.coords)
                           (.startswith line "/assess") (assess messages player.coords line)
                           (.startswith line "/what-if") (what-if messages player.coords line)
                           (.startswith line "/hint") (hint messages player.coords line)
                           (.startswith line "/forget") (setv messages [])
                           (go? line) (move messages player line)
                           (command? line) (error "I didn't understand that command.")
                           line (narrate (append user-msg messages) player))]
          (when result
            (save-messages messages)
            (print-message result)
            (.extend messages [user-msg result])))
        (except [KeyboardInterrupt]
          (print)
          (error "**/quit** to exit"))
        (except [e [Exception]]
          (log.error "REPL error" e :mode "w" :logfile "traceback.log")
          (exception)
          (sleep 5))))
    (save-messages messages)
    (readline.write-history-file history-file)
    (clear)))
