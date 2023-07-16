"
The server, implementing RPC for engine functions.
Protocol: see wire.hy.
"
(require hyrule [defmain])

(import sys)
(import json)
(import datetime [datetime])

(import zmq)

(import chasm [log])

(import chasm [engine state crypto])
(import chasm.stdlib [config hash-id])
(import chasm.wire [wrap unwrap])


(setv fns {"spawn" engine.spawn-player
           "parse" engine.parse})

;;; -----------------------------------------------------------------------------

(setv context (zmq.Context)
      socket (.socket context zmq.REP))

(.bind socket (config "listen"))

         
;;; -----------------------------------------------------------------------------

(defn auth [player-name pub-key]
  "Store the public key if it's not already known. Return the stored public key."
  (let [account (state.get-account player-name)]
    (if account
        (:key account None)
        (:key (state.update-account player-name :key pub-key)))))

(defn serve []
  "Call a function on the server."
  (log.info f"Starting server at {(.isoformat (datetime.today))}")
  (while True
    (try
      (let [msg (unwrap (.recv-string socket))
            player (:player msg {})
            payload (:payload msg {})
            function (:function payload [])
            args (:args payload [])
            kwargs (:kwargs payload [])
            pub-key (auth player (:public-key msg "")) ; TODO: auth/lookup here
            signature (:signature msg "")
            client-time (:sender-time msg) ; TODO: discard if too old
            expected-hash (hash-id (+ client-time (json.dumps payload)))]
        (log.debug f"server/serve: {msg}")
        (if (crypto.verify pub-key signature expected-hash)
          (let [response ((get fns function) #* args #** kwargs)]
            (.send-string socket (wrap response)))
          (.send-string socket (wrap {"errors" f"failed to verify signature:\n{payload}\n\n{pub-key}\n{signature}\n{expected-hash}"}))))
      (except [KeyError]
        (log.error f"server/serve: msg")))))

(defmain []
  (sys.exit (serve)))
