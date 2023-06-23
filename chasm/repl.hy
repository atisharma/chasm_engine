"
The main REPL where we read output and issue commands.
"
(require hyrule.argmove [-> ->> as->])

(import chasm [log])

(import os)
(import datetime [datetime])
(import readline)

(import chasm.stdlib *)
(import chasm [place character])
(import chasm.types [Coords])
(import chasm.chat [chat append prepend user assistant])
(import chasm.interface [banner
                         clear
                         console
                         spinner
                         clear-status-line
                         status-line
                         print-message
                         print-messages
                         format-msg
                         info
                         error
                         exception])


;; TODO: cache recent locations, adjust verbosity accordingly

(defn is-quit [line]
  (or (.startswith line "/q")
      (.startswith line "/exit")))

(defn is-look [line]
  (.startswith line "/l"))

(defn is-hist [line]
  (.startswith line "/hi"))

(defn is-go [line]
  (let [[cmd _ dirn] (.partition line " ")]
    (when (.startswith cmd "/g")
      dirn)))
  
(defn move [coords dirn]
  "Extend the map. Adjust the coordinates. Describe. Return msg-pair."
  (let [new-coords (place.go dirn coords)]
    (when new-coords
      (place.extend-map new-coords)
      {"coords" new-coords
       "msg" (assistant (place.describe new-coords :paragraphs 0))})))

(defn look [coords]
  "Describe the surroundings."
  (assistant (place.describe coords :paragraphs 1)))

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
                 (character.spawn username :box [1 1]))
        messages []]
    (try
      (readline.read-history-file history-file)
      (except [e [FileNotFoundError]]))
    (while True
      (clear-status-line)
      (status-line f"{(:x coords)} {(:y coords)} | [italic cyan]{(place.name coords)}[/italic cyan]")
      (try ; ----- parser block ----- 
        (let [line (.strip (rlinput "> "))
              dirn (go? line)
              user-msg (user line)
              result (cond (quit? line) (break)
                           (look? line) (look coords)
                           (hist? line) (print-messages messages)
                           dirn (let [x (move coords dirn)]
                                  (setv coords (:coords x))
                                  (:msg x))
                           line (chat (append user-msg messages)))]
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
          (raise e))))
    (readline.write-history-file history-file)
    (clear)))
    
