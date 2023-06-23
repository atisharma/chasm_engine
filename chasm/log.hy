(import logging)

;; overrides root logger to capture logs of any badly-behaved imported modules
(logging.basicConfig :filename "chasm.log"
                     :level logging.WARNING
                     :encoding "utf-8")

(defn debug [msg]
  (logging.debug msg))

(defn info [msg]
  (logging.info msg))

(defn warn [msg]
  (logging.warn msg))

(defn error [msg]
  (logging.error msg))
