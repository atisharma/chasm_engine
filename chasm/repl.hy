"
The main REPL where we read output and issue commands.
"

(require hyrule.argmove [-> ->> as->])

(import chasm [log])

(import os)
(import datetime [datetime])
(import time [sleep])

(import chasm.stdlib *)
(import chasm.engine [parse spawn-player])
(import chasm.chat [msgs->dlg])
(import chasm.interface [banner clear console pinput spinner
                         exception error info print-message print-messages
                         status-line
                         _bg _fg _it])


;;; -----------------------------------------------------------------------------
;;; All player-specific code goes in here
;;; -----------------------------------------------------------------------------

(defn status [response]
  "Show game, place, inventory..."
  (let [world-name (:world response)
        coords (:coords response)
        p (:player response)
        name (:name p)
        inventory (.join ", " (:inventory p))
        place-name (:place p)
        score (:score p)
        objectives (:objectives p)]
    (status-line
      (.join "\n"
             [(.join " | "
                     [(_bg (_it f"{world-name}") :col "green")
                      (_bg (_it f"{place-name}") :col "cyan")
                      (_it f"{(:x coords)} {(:y coords)}")])
              (.join " | "
                     [(_bg (_it f"{name}") :col "green")
                      (_bg (_it f"{objectives}") :col "red")
                      (_it f"score: {score}")])
              (_it f"{inventory}")]))))

(defn run []
  "Launch the REPL, which takes player's input, parses
it, and passes it to the appropriate action."
  (log.info f"Starting repl at {(.isoformat (datetime.today))}")

  (banner)
  (console.rule)

  (let [player-name (config "name")
        card-path f"characters/{player-name}.json"
        player-attributes (or (load card-path) {})
        response (with [(spinner "Spawning...")]
                   (spawn-player player-name #** player-attributes))]
    (while True
      (try ; ----- parser block ----- 
        (let [message (:result response)]
          (when message
            (match (:role message)
                   "QUIT" (do (clear) (break))
                   "assistant" (print-message message)
                   "info" (info (:content message))
                   "error" (error (:content message))
                   "history" (print-messages (msgs->dlg player-name "narrator" (:narrative response))))))
        (status response)
        (let [line (.strip (pinput "> "))]
          (setv response (with [(spinner "Writing...")]
                           (parse player-name line))))
        (except [KeyboardInterrupt]
          (print)
          (error "**/quit** to exit"))
        (except [e [Exception]]
          (log.error "REPL error" e :mode "w" :logfile "traceback.log")
          (exception)
          (sleep 10))))))
