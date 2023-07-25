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
                     :format "%(asctime)s : %(levelname)s : %(module)s/%(funcName)s : %(message)s"
                     :encoding "utf-8")

(logging.info (* "-" 80))

(setv debug logging.debug)
(setv info logging.info)
(setv warn logging.warn)

(defn error [msg [exception None] [mode "a"] [logfile logfile]]
  (logging.error msg)
  (when exception
    (with [f (open logfile :mode mode :encoding "UTF-8")]
      (import traceback)
      (traceback.print-exception exception :file f))))
