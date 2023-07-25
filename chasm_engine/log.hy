(require hyrule.argmove [-> ->>])

(import logging)

(import chasm_engine.stdlib *)

;; TODO: per-module log configuration


(setv log-name (-> (config "world")
                   (.split "/")
                   (last)))

(setv logfile (or (config "logfile") f"{log-name}.log"))


;; overrides root logger to capture logs of any badly-behaved imported modules
(logging.basicConfig :filename logfile
                     :level (getattr logging (.upper (or (config "loglevel") "WARNING")))
                     :encoding "utf-8")

(logging.info (* "-" 80))

(defn debug [msg]
  (logging.debug msg))

(defn info [msg]
  (logging.info msg))

(defn warn [msg]
  (logging.warn msg))

(defn error [msg [exception None] [mode "a"] [logfile logfile]]
  (logging.error msg)
  (when exception
    (with [f (open logfile :mode mode :encoding "UTF-8")]
      (import traceback)
      (traceback.print-exception exception :file f))))