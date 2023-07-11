"
The main REPL where we read output and issue commands.

TODO: async loop? Develop characters in the background?
"

(require hyrule.argmove [-> ->> as->])

(import chasm [log])

(import os)
(import datetime [datetime])
(import readline)
(import time [sleep])

(import chasm.stdlib *)
(import chasm.engine *)
; TODO: just expose engine, move calls to these three modules into there.
(import chasm [place item character])
(import chasm.types [Coords])
(import chasm.state [world world-name news username
                     get-character])
(import chasm.chat [token-length
                    load-messages save-messages
                    truncate
                    user assistant system
                    ChatError])
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
             ;f"*{(place.nearby-str coords :list-inaccessible True)}*"
             ;f"Of which are accessible:"
             f"*{(place.nearby-str coords)}*"])))

(defn spy [char-name]
  (info
    (character.describe
      (get-character char-name)
      :long True)))

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
  (let [inventory (.join ", " (lfor i (item.inventory player) i.name))]
    (clear-status-line)
    (status-line
      (.join "\n"
             [(.join " | "
                     [f"[italic blue]{world-name}[/italic blue]"
                      f"[italic cyan]{(place.name player.coords)}[/italic cyan]"
                      f"{(:x player.coords)} {(:y player.coords)}"
                      f"{(+ (token-length world) (token-length messages))} tkns"])
              (.join " | "
                     [f"[italic blue]{player.name}[/italic blue]"
                      f"[italic red]{player.objectives}[/italic red]"
                      f"[italic cyan]score: {player.score}[/italic cyan]"])
              f"[italic magenta]{inventory}[/italic magenta]"]))))

(defn run []
  "Launch the REPL, which takes player's input, parses
it, and passes it to the appropriate action."
  (log.info f"Starting repl at {(.isoformat (datetime.today))}")
  (banner)
  ;(info "Enter **/help** for help\n")
  (console.rule)
  (let [history-file (os.path.join (os.path.expanduser "~") ".chasm_history")
        player (with [s (spinner "Spawning...")]
                 (character.spawn :name username))
        messages (or (load-messages player)
                     (with [s (spinner "Writing...")]
                       [(describe-place player)]))
        counter (count)]
    (try
      (readline.set-history-length 100)
      (readline.read-history-file history-file)
      (except [e [FileNotFoundError]]))
    (print-message (last messages))
    (while True
      (try ; ----- parser block ----- 
        (setv messages (truncate messages :spare-length (+ (token-length world) (config "max_tokens")))) 
        (setv player (get-character player.name)) ; reload every loop to sync from db
        (status messages player)
        (let [line (.strip (rlinput "> "))
              user-msg (user line)
              result (cond (.startswith line "!") (parse-hy line)
                           (quit? line) (break)
                           (look? line) (look player :messages messages)
                           (take? line) (info (item.fuzzy-claim (take? line) player))
                           (drop? line) (info (item.fuzzy-drop (drop? line) player))
                           (hist? line) (print-messages messages)
                           (.startswith line "/banner") (banner)
                           (.startswith line "/clear") (clear)
                           (.startswith line "/items") (info (item.describe-at player.coords))
                           (.startswith line "/characters") (info (or (character.describe-at player.coords) "Nobody interesting here but you."))
                           (.startswith line "/spy") (spy (last (.partition line " ")))
                           (.startswith line "/map") (print-map player.coords)
                           (.startswith line "/what-if") (info (narrate (append (user (last (.partition line))) messages) player))
                           (.startswith line "/hint") (hint messages player line)
                           (.startswith line "/forget") (do (banner)
                                                            (setv messages [])
                                                            (describe-place player))
                           (go? line) (move (append user-msg messages) player)
                           (talk? line) (converse (append user-msg messages) player)
                           (command? line) (error "I didn't understand that command.")
                           line (narrate (append user-msg messages) player))]
          (when result
            (print-message result)
            (.extend messages [user-msg result])
            (save-messages player messages)
            (move-characters messages player)
            (when (and messages
                       (not (% (next counter) 2))
                       ; don't develop when moving
                       (not (go? line)))
              (develop messages player))))
        (except [KeyboardInterrupt]
          (print)
          (error "**/quit** to exit"))
        (except [ChatError]
          (exception))
        (except [e [Exception]]
          (log.error "REPL error" e :mode "w" :logfile "traceback.log")
          (exception)
          (sleep 10))))
    (save-messages player messages)
    (readline.write-history-file history-file)
    (clear)))
