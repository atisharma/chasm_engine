"
The server, implementing RPC for engine functions.
Protocol: see wire.hy.
"
(require hyrule [defmain])

(import sys)
(import json)
(import datetime [datetime])

(import zmq [Context REP])

(import chasm [log])

(import chasm [engine])
(import chasm.wire [wrap unwrap])


;;; -----------------------------------------------------------------------------

(setv context (Context)
      socket (.socket context REP))

(.bind socket (config "listen"))

;;; -----------------------------------------------------------------------------

(defn serve []
  "Call a function on the server."
  (log.info f"Starting server at {(.isoformat (datetime.today))}")
  (while True
    (try
      (let [fns {"spawn" engine.spawn-player
                 "parse" engine.parse}
            msg (unwrap (.recv-string socket))
            function (:function msg [])
            args (:args msg [])
            kwargs (:kwargs msg [])]
        (log.info f"server/serve: {msg}")
        (let [response ((get fns function) #* args #** kwargs)]
          (.send-string socket (wrap response))))
      (except [KeyError]
        (log.error f"server/serve: msg")))))

(defmain []
  (sys.exit (serve)))
