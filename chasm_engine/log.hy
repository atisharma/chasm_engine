(require hyrule.argmove [-> ->>])

(import logging)
(import logging.config [dictConfig])

(import chasm_engine.stdlib *)

;; TODO: per-module log configuration

(setv logger (logging.getLogger __name__))

(setv log-name (-> (config "world")
                   (.split "/")
                   (last)))
(setv logfile (or (config "logfile") f"{log-name}.log"))
(setv loglevel (.upper (or (config "loglevel") "WARNING")))
(setv handler (logging.FileHandler :filename logfile))

(handler.setFormatter (logging.Formatter "%(asctime)s : %(levelname)s : %(module)s/%(funcName)s : %(message)s"))
(logger.addHandler handler)
(logger.setLevel loglevel)

(logger.info (* "-" 80))

(setv debug logger.debug)
(setv info logger.info)
(setv warn logger.warn)

(defn error [msg [exception None] [mode "a"] [logfile logfile]]
  (logger.error msg)
  (when exception
    (with [f (open logfile :mode mode :encoding "UTF-8")]
      (import traceback)
      (traceback.print-exception exception :file f))))
