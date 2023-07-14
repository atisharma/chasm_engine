"
Functions that relate to output on the screen.
"

(require hyrule.argmove [-> ->> as->])
(require hyrule.control [case])

(import os)
(import re)
(import atexit)
(import readline)
(import hashlib [md5])

(import rich.console [Console])
(import rich.padding [Padding])
(import rich.markdown [Markdown])
(import rich.columns [Columns])
(import rich.table [Table])
(import rich.text [Text])
(import rich.progress [track])
(import rich.color [ANSI_COLOR_NAMES])

(import prompt-toolkit [HTML prompt :as pprompt])


;;; -----------------------------------------------------------------------------

(setv console (Console :highlight None))
(setv colors (list (.keys ANSI_COLOR_NAMES)))
(setv render-markdown True)

;; status bar
(setv toolbar "")

;;; -----------------------------------------------------------------------------
;;; startup/exit
;;; -----------------------------------------------------------------------------

;; load/save readline history at startup/exit

(try
  (let [history-file (os.path.join (os.path.expanduser "~") ".chasm_history")]
    (readline.set-history-length 100)
    (readline.read-history-file history-file)
    (.register atexit readline.write-history-file history-file))
  (except [e [FileNotFoundError]]))

;;; -----------------------------------------------------------------------------
;;; Input
;;; -----------------------------------------------------------------------------

(defn rlinput [prompt [prefill ""]]
  "Like python's input() but using readline."
  (readline.set_startup_hook (fn [] (readline.insert_text prefill)))
  (try
    (input prompt)
    (except [EOFError]
      "/quit")
    (finally
      (readline.set_startup_hook))))

(defn pinput [prompt]
  "Input with prompt-toolkit."
  (pprompt prompt :bottom-toolbar (HTML toolbar)))

(defn _it [s]
  (+ "<i>" s "</i>"))

(defn _tag [s [tag "i"]]
  (+ f"<{tag}>" s f"</{tag}>"))

(defn _fg [s [col "default"]]
  (let [tag f"t_fg_{col}"]
    (+ f"<{tag} fg=\"{col}\">" s f"</{tag}>")))

(defn _bg [s [col "default"]]
  (let [tag f"t_bg_{col}"]
    (+ f"<{tag} bg=\"{col}\">" s f"</{tag}>")))

;;; -----------------------------------------------------------------------------
;;; Screen control
;;; -----------------------------------------------------------------------------

(defn clear []
  (console.clear))
  
(defn set-width [line]
  (try
    (let [arg (get (.partition line " ") 2)]
      (global console)
      (setv console (Console :highlight None :width (int arg))))
    (except [[IndexError ValueError]]
      (error "Bad console width value."))))

(defn toggle-markdown []
  "Toggle the rendering of markdown in output."
  (global render-markdown)
  (setv render-markdown (not render-markdown)))

(defn spinner [s [style "italic blue"] [spinner "dots12"]]
  (console.status (Text s :style style)
                  :spinner spinner))

;;; -----------------------------------------------------------------------------
;;; Printers
;;; -----------------------------------------------------------------------------

(defn info [s [style "blue italic"]]
  "Print an information string to the screen."
  (print-markdown s :style style))

(defn error [s [style "red italic"]]
  "Print an error string to the screen."
  (print-markdown s :style style))

(defn exception []
  "Formats and prints the current exception."
  (console.print-exception :max-frames 2))

;;; -----------------------------------------------------------------------------

(defn banner []
  (console.clear)
  (setv banner-text r"       _:
   ___| |__   __ _ ___ _ __ ___ :
  / __| '_ \ / _` / __| '_ ` _ \ :
 | (__| | | | (_| \__ \ | | | | | :
  \___|_| |_|\__,_|___/_| |_| |_| :
 :")
  (lfor [l c] (zip (.split banner-text ":")
                   ["#11FF00" "#33DD00" "#33BB00" "#339900" "#337720" "#227799" "#2288FF" "#2288FF"])
        (console.print l
                       :end None
                       :style f"bold {c}"
                       :overflow "crop"))
  (console.print "[default]"))

(defn print-markdown [s [style None] [padding #(0 3 0 0)]]
  "Print some markdown to the screen."
  (print "\033[K" :end "") ; clear to end of line for new input
  (-> s
      (sanitize-markdown)
      (Markdown)
      (Padding padding)
      (console.print :justify "left" :style style)))

(defn status-line [s]
  "Print a status line at the bottom of the screen."
  ;(print "\033[s" :end "") ; save cursor position
  ;(print "\033[u" :end "") ; restore cursor position
  #_(print) ; move on one line
  #_(console.rule)
  ; s-without-markup (re.sub r"\[[/\w ]*\]" "" s)
  #_(console.print s
                   :end "\r"
                   :overflow "ellipsis"
                   :crop True)
  #_(for [n (range (+ 2 (.count s "\n")))]
      (print "\033[1A" :end "")) ; up one line
  #_(print "\033[K" :end "") ; clear to end of line for new input
  (global toolbar)
  (setv toolbar s))
  
(defn clear-status-line []
  "Hack to avoid old status line polluting new output."
  #_(print "\033[K" :end "") ; clear to end of line
  #_(print)
  #_(print "\033[K" :end "") ; clear to end of line
  #_(print)
  #_(print "\033[K" :end "") ; clear to end of line
  #_(print)
  #_(print "\033[1A" :end "") ; up one line
  #_(print "\033[1A" :end "") ; up one line
  #_(print "\033[1A" :end "")) ; up one line
  
(defn role-color [role]
  "The signature color of the role, derived from its name."
  (let [role (.capitalize role)
        i (-> (role.encode "utf-8")
              (md5)
              (.hexdigest)
              (int 16)
              (% 222)
              (+ 1))]
    (get colors i)))

(defn print-messages [messages]
  "Format and print messages to the terminal."
  (console.rule)
  (console.print)
  (for [msg messages]
    (print-message msg :padding #(0 4 1 0)))
  (console.rule))

(defn print-message [msg [padding #(0 3 1 0)]]
  "Format and print a message with role to the screen."
  (let [color (role-color (:role msg))
        output (Table :padding padding
                      :expand True
                      :show-header False
                      :show-lines False
                      :box None)
        role-prompt (case (:role msg)
                          "assistant" ""
                          "user" "> "
                          "system" ""
                          else f"{(:role msg)}: ")]
    (.add-column output :min-width 2)
    (.add-column output :ratio 1 :overflow "fold")
    (.add-row output f"[bold {color}]{role-prompt}[/bold {color}]"
              (if render-markdown
                  (Markdown (sanitize-markdown (:content msg)))
                  (:content msg)))
    (console.print output :justify "left")))

(defn print-last-message [messages] 
  (-> messages
    (get -1)
    (print-message))) 

(defn tabulate [rows headers
                [styles None]
                [title None]]
  "Print a rich table object from a list of lists (rows) and a list (headers)."
  (let [table (Table :title title :row-styles styles)]
    (for [h headers]
      (.add-column table h))
    (for [r rows]
      (.add-row table #* r))
    (console.print table :style "green")))

;;; -----------------------------------------------------------------------------
;;; Formatters
;;; -----------------------------------------------------------------------------

(defn sanitize-markdown [s]
  "Prepare a generic string for markdown rendering."
  ;; Markdown swallows single newlines.
  ;; and defines the antipattern of preserving them with a double space.
  ;; but we don't want to lose itemised lists
  (re.sub r"\n" r"  \n" (.strip s)))
