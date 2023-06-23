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


;; TODO: cache recent locations, adjust verbosity accordingly

;;; -----------------------------------------------------------------------------

(defn is-quit [line]
  (or (.startswith line "/q")
      (.startswith line "/exit")))

(defn is-look [line]
  (.startswith line "/l"))

(defn is-hist [line]
  (.startswith line "/hi"))

(defn is-go [line]
  (let [[cmd _ dirn] (.partition line " ")]
    (cond (.startswith cmd "/g") dirn
          (in cmd ["/n" "/e" "/s" "/w" "/ne" "/se" "/sw" "/nw"]) (rest cmd))))
  
;;; -----------------------------------------------------------------------------

(defn parse [line]
  "Lines starting with `!` are sent here.
Evaluate a Hy form."
  (let [result (-> line
                   (rest)
                   (hy.reader.read)
                   (hy.eval))]
    (when result
      (info result :style "green"))))

(defn move [coords dirn]
  "Extend the map. Adjust the coordinates. Describe. Return msg-pair."
  (let [new-coords (place.go dirn coords)]
    (when new-coords
      (with [s (spinner "Writing...")]
        (place.extend-map new-coords)
        {"coords" new-coords
         "msg" (assistant (.join "\n\n"
                                 [f"**{(place.name new-coords)}**"
                                  (place.describe new-coords :paragraphs 0)]))}))))

(defn look [coords [paragraphs 2]]
  "Describe the surroundings."
  ; TODO: speak of characters and items. Look at a character.
  (with [s (spinner "Writing...")]
    (assistant (place.describe coords :paragraphs paragraphs))))

(defn print-map [coords]
  "Get your bearings."
  (print-message (system (.join "\n\n"
                             [f"***{(place.name coords)}***"
                              f"*{(place.nearby-str coords)}*"]))))

(defn converse [messages coords]
  "Carry on a conversation, in the fictional universe."
  (let [story-guidance "The following is a passage from a gripping and enjoyable story.
The reader interjects with questions. These are meant in the context of the story.
You respond as the author. You must never break the 'fourth wall'. Ignore off-topic questions.

Story setting: {world}"
        location-guidance f"{(news)}
You are at {(place.name coords)}. {(place.rooms coords)}.
Nearby are:
{(place.nearby-str coords)}"]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system location-guidance))
           (respond)
           (trim-prose)
           (assistant)))))

(defn status [coords messages]
  (clear-status-line)
  (status-line
    (.join " | "
           [f"[italic blue]{world-name}[/italic blue]"
            f"[italic cyan]{(place.name coords)}[/italic cyan]"
            f"[italic magenta]{(.join ", " (place.rooms coords :as-string False))}[/italic magenta]"
            f"{(:x coords)} {(:y coords)}"
            f"{(+ (token-length world) (token-length messages))} tkns"])))


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
                                     (place.describe coords :paragraphs 1)]))]]
    (try
      (readline.set-history-length 100)
      (readline.read-history-file history-file)
      (except [e [FileNotFoundError]]))
    (console.print)
    (print-message (system f"*{world}*"))
    (print-message (last messages))
    (while True
      (try ; ----- parser block ----- 
        (setv messages (truncate messages
                                 :length (- (config "context_length")
                                            (token-length world)
                                            750))) 
        (status coords messages)
        (let [line (.strip (rlinput "> "))
              dirn (go? line)
              user-msg (user line)
              result (cond (.startswith line "!") (parse line)
                           (quit? line) (break)
                           (look? line) (look coords)
                           (.startswith line "/map") (print-map coords)
                           (.startswith line "/system") (print-message (system (.join "\n"[world (news)])))
                           (hist? line) (print-messages messages)
                           dirn (let [x (move coords dirn)]
                                  (setv coords (:coords x))
                                  (:msg x))
                           (.startswith line "/") (error "I didn't understand your command.")
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
          (sleep 30))))
          ;(raise e))))
    (readline.write-history-file history-file)
    (clear)))
