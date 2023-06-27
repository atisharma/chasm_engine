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
(import chasm.constants [compass-directions])
(import chasm [place item character])
(import chasm.types [Coords])
(import chasm.state [world world-name news username])
(import chasm.chat [token-length
                    load-messages save-messages
                    truncate append prepend
                    user assistant system])
(import chasm.engine [move look hint assess what-if narrate])
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

(defn take? [line] ; -> item or None
  "Are you trying to pick up an item?"
  (let [[cmd _ obj] (.partition line " ")]
    (cond (.startswith cmd "/take") (sstrip obj)
          (= cmd "take") (sstrip obj)
          (= cmd "pick") (re.sub "^up " "" (sstrip obj)))))

(defn drop? [line coords] ; -> item or None
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

(defn status [messages player coords]
  "Show game, place, coords, tokens used."
  (let [l (+ 30 (len f"{world-name}{(place.name coords)}"))]
    (clear-status-line)
    (status-line
      (.join "\n"
             [(.join " | "
                     [f"[italic blue]{world-name}[/italic blue]"
                      f"[italic cyan]{(place.name coords)}[/italic cyan]"
                      f"{(:x coords)} {(:y coords)}"
                      f"{(+ (token-length world) (token-length messages))} tkns"])
              f"[italic magenta]{(character.inventory player)}[italic magenta]"]))))

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
        coords player.coords
        _p (with [s (spinner "Building world...")]
             (place.extend-map coords)) ; ensure there's somwhere to go
        messages (or (load-messages)
                     (with [s (spinner "Writing...")]
                       [(assistant (.join "\n\n"
                                          [f"**{(place.name coords)}**"
                                           (place.describe coords :length "very short")]))]))]
    (log.warn player)
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
        (status messages player coords)
        (let [line (.strip (rlinput "> "))
              user-msg (user line)
              result (cond (.startswith line "!") (parse-hy line)
                           (quit? line) (break)
                           (look? line) (look coords :messages messages)
                           (hist? line) (print-messages messages)
                           (.startswith line "/items") (info (item.describe-at coords))
                           (.startswith line "/characters") (info (or (character.describe-at coords) "Nobody here but you."))
                           (.startswith line "/map") (print-map coords)
                           (.startswith line "/assess") (assess messages coords line)
                           (.startswith line "/what-if") (what-if messages coords line)
                           (.startswith line "/hint") (hint messages coords line)
                           (.startswith line "/forget") (setv messages [])
                           ;(.startswith line "/system") (print-message (system (.join "\n"[world (news)])))
                           (go? line) (let [x (move coords (go? line) player :messages messages)]
                                        (if x
                                            (do (setv coords (:coords x))
                                                (:msg x))
                                            (narrate (append user-msg messages) player)))
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
          (with [f (open "traceback.log" :mode "w" :encoding "UTF-8")]
            (import traceback)
            (traceback.print-exception e :file f))
          (exception)
          (sleep 5))))
    (save-messages messages)
    (readline.write-history-file history-file)
    (clear)))
