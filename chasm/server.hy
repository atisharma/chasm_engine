"
The server, implementing RPC for engine method.
Protocol: see wire.hy.
"
(require hyrule [defmain unless])

(import sys)
(import json)
(import time [time])
(import datetime [datetime])

(import zmq)

(import chasm [log])

(import chasm [engine crypto])
(import chasm.stdlib [config hash-id inc])
(import chasm.state [get-account set-account update-account])
(import chasm.wire [wrap unwrap zerror])


; this defines what RPCs are available to the client
(setv server-methods {"spawn" engine.spawn-player
                      "parse" engine.parse
                      "motd" engine.motd
                      "null" engine.null})

;;; -----------------------------------------------------------------------------

(setv context (zmq.Context))

(defn start-server-socket []
  (setv socket (.socket context zmq.REP))
  ; see https://stackoverflow.com/questions/26915347/zeromq-reset-req-rep-socket-state
  ; server will tick every 2s
  (.setsockopt socket zmq.RCVTIMEO 2000)
  (.setsockopt socket zmq.SNDTIMEO 1000)
  (.setsockopt socket zmq.LINGER 2000)
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
  (let [account (get-account player-name)]
    (if (and account (:ecdsa-key account None)) ; if there is an account and it has a key
        (:ecdsa-key account) ; use the key, or
        (:ecdsa-key (update-account player-name
                                    :name player-name
                                    :last-accessed (time)
                                    :ecdsa-key pub-key))))) ; store the provided key

(defn verify [msg]
  "Check the message signature."
  (let [player-name (:player msg {})
        payload (:payload msg {})
        stored-pub-key (auth player-name (:public-key msg ""))
        signature (:signature msg "")
        client-time (:sender-time msg Inf)
        expected-hash (hash-id (+ client-time (json.dumps payload)))]
    (crypto.verify stored-pub-key signature expected-hash))) ; check the signature

(defn time-ok? [client-time [threshold 120]]
  "Is client's message time within threshold (seconds) of server time?"
  (try
    (let [ct (float client-time)
          diff (abs (- ct (time)))]
      (< diff threshold))
    (except [ValueError])))

(defn handle-request [player-name client-time method #* args #** kwargs]
  "Process the RPC in the engine and send the result."
  (let [account (get-account player-name)]
    (update-account player-name :last-verified client-time)
    ((.get server-methods method "null") #* args #** kwargs)))

(defn serve []
  "Call a method on the server."
  (print f"Starting server at {(.isoformat (datetime.today))}")
  (log.info f"Starting server at {(.isoformat (datetime.today))}")
  (print "Initial map generation...")
  (engine.init)
  (print "Ready for players.")
  (while True
    (try
      (let [msg (unwrap (.recv-string socket)) ; no messages will raise zmq.Again
            player-name (:player msg {})
            payload (:payload msg {})
            method (:method payload "null") 
            args (:args payload [])
            kwargs (:kwargs payload [])
            client-time (:sender-time msg Inf)]
        (log.debug f"server/serve: {msg}")
        (send
          (cond
            (not (time-ok? client-time)) (zerror "STALE" f"Message stale, off by {(int (- (float client-time) (time)))}s, server was probably busy.")
            (verify msg) (handle-request player-name client-time method #* args #** kwargs)
            :else (zerror "SIGNATURE" "Failed to verify signature. Maybe your name/passphrase is wrong."))))
      (except [zmq.Again]
        ; only process world if there is no message queue
        (or (engine.extend-world)
            (engine.develop)
            (engine.spawn-characters)
            (engine.spawn-items)
            (engine.set-offline-players)))
      (except [KeyError]
        (log.error f"server/serve: {msg}"))
      (except [KeyboardInterrupt]
        (print "Interrupted, quitting.")
        (log.info f"server/serve: quit")
        (break)))))

(defmain []
  (sys.exit (or (serve) 0)))
