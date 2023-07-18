"
The server, implementing RPC for engine functions.
Protocol: see wire.hy.
"
(require hyrule [defmain])
(require hyrule [unless])

(import sys)
(import json)
(import time [time])
(import datetime [datetime])

(import zmq)

(import chasm [log])

(import chasm [engine state crypto])
(import chasm.stdlib [config hash-id])
(import chasm.wire [wrap unwrap])


; this defines what RPCs are available to the client
(setv server-functions {"spawn" engine.spawn-player
                        "parse" engine.parse
                        "null" engine.null})

;;; -----------------------------------------------------------------------------

(setv context (zmq.Context))

(defn start-server-socket []
  (setv socket (.socket context zmq.REP))
  ; see https://stackoverflow.com/questions/26915347/zeromq-reset-req-rep-socket-state
  ; server will tick every 1s
  (.setsockopt socket zmq.RCVTIMEO 1000)
  (.setsockopt socket zmq.SNDTIMEO 1000)
  (.setsockopt socket zmq.LINGER 1000)
  (.bind socket (config "listen"))
  socket)

(setv socket (start-server-socket))

         
;;; -----------------------------------------------------------------------------

(defn send [msg]
  (try
    (.send-string socket (wrap msg))
    (except [zmq.EAGAIN]
      (log.error "server/send: client send failed."))))
  
(defn auth [player-name pub-key]
  "Store the public key if it's not already known. Return the stored public key. First-come first-served."
  (let [account (state.get-account player-name)]
    (if (and account (:ecdsa-key account None)) ; if there is an account and it has a key
        (:ecdsa-key account) ; use the key, or
        (:ecdsa-key (state.update-account player-name :ecdsa-key pub-key))))) ; store the provided key

(defn time-ok? [client-time [threshold 30]]
  "Is client time within threshold (seconds) of server time?"
  (try
    (let [ct (float client-time)
          diff (abs (- ct (time)))]
      (< diff threshold))
    (except [ValueError])))

(defn handle-request [player-name client-time function #* args #** kwargs]
  "Process the RPC in the engine and send the result."
  (let [account (state.get-account player-name)]
    (state.update-account player-name :last-verified client-time :turns (inc (:turns account 0)))
    ((.get server-functions function "null") #* args #** kwargs)))

(defn serve []
  "Call a function on the server."
  (log.info f"Starting server at {(.isoformat (datetime.today))}")
  (while True
    (try
      (let [msg (unwrap (.recv-string socket)) ; no messages will raise zmq.Again
            player-name (:player msg {})
            payload (:payload msg {})
            function (:function payload "null") 
            args (:args payload [])
            kwargs (:kwargs payload [])
            stored-pub-key (auth player-name (:public-key msg ""))
            signature (:signature msg "")
            client-time (:sender-time msg)
            expected-hash (hash-id (+ client-time (json.dumps payload)))]
        (log.debug f"server/serve: {msg}")
        (send
          (cond
            (not (time-ok? client-time)) {"errors" f"Bad message time, got {client-time}, expected {(time)}"}
            (crypto.verify stored-pub-key signature expected-hash) (handle-request player-name client-time function #* args #** kwargs)
            :else {"errors" "Failed to verify signature."})))
      (except [zmq.Again]
        ; only process world if there is no message queue
        (engine.extend-world)
        (engine.develop))
      (except [KeyError]
        (log.error f"server/serve: {msg}"))
      (except [KeyboardInterrupt]
        (log.info f"server/serve: quit")
        (break)))))

(defmain []
  (sys.exit (or (serve) 0)))
