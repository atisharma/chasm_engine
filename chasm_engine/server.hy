"
The server, implementing RPC for engine method.
Protocol: see wire.hy.
See documentation:
- https://api.zeromq.org
- https://zeromq.org/get-started
- https://zguide.zeromq.org
"

(require hyrule [defmain unless])

(require hyjinx.macros [prepend append])

(import sys)
(import asyncio)
(import json)
(import time [time])
(import datetime [datetime])

(import zmq)
(import zmq.asyncio)

(import hyjinx [crypto])

(import chasm_engine [log])

(import chasm_engine [engine])
(import chasm_engine.lib [config-file config hash-id inc])
(import chasm_engine.state [get-account set-account update-account])
(import chasm_engine.wire [wrap unwrap zerror])


(setv N_CONCURRENT_CLIENTS 1000
      BACKGROUND_TICK 1)

;; -----------------------------------------------------------------------------

(setv context (zmq.asyncio.Context))

(defn start-router-socket [address]
  "Set up a ROUTER-REQ connection."
  (setv socket (.socket context zmq.ROUTER))
  ; see https://stackoverflow.com/questions/26915347/zeromq-reset-req-rep-socket-state
  ; server will tick every 2s
  (.setsockopt socket zmq.RCVTIMEO 2000)
  (.setsockopt socket zmq.SNDTIMEO 1000)
  (.setsockopt socket zmq.LINGER 2000)
  (.bind socket address)
  socket)

(setv frontend (start-router-socket (config "listen")))
(log.info (config "listen"))

;; -----------------------------------------------------------------------------

(defn :async send [zmsg]
  (try
    (await (.send-multipart frontend zmsg))
    (except [zmq.EAGAIN]
      (log.error "client send failed."))))
  
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

(defn :async handoff-request [player-name client-time method #* args #** kwargs]
  "Process the RPC in the engine and send the result."
  (let [account (get-account player-name)]
    (update-account player-name :last-verified client-time)
    (match method
           "spawn" (await (engine.spawn-player #* args #** kwargs))
           "parse" (await (engine.parse #* args #** kwargs))
           "motd" (engine.motd #* args #** kwargs)
           "status" (engine.status #* args #** kwargs)
           "online" (engine.online #* args #** kwargs)
           "null" (engine.null #* args #** kwargs))))

;; TODO: handle incoming requests separately from sending the replies
;; That is, all output is either 'ACK' or streamed output
(defn :async handle-frames [frames]
  "Unwrap, verify an incoming message."
  (let [[q ident z zmsg] frames
        msg (unwrap zmsg) ; no messages will raise zmq.Again
        player-name (:player msg {})
        payload (:payload msg {})
        method (:method payload "null") 
        args (:args payload [])
        kwargs (:kwargs payload [])
        client-time (:sender-time msg Inf)
        response (cond (not (time-ok? client-time)) (zerror "STALE" f"Message stale, off by {(int (- (float client-time) (time)))}s, server was probably busy or your clock is wrong.")
                       (verify msg) (await (handoff-request player-name client-time method #* args #** kwargs))
                       :else (zerror "SIGNATURE" "Failed to verify signature. Maybe your name/passphrase is wrong."))]
    (log.debug f"{msg}")
    (await (send [q ident z (wrap response)]))))

(defn :async server-loop [[n 0]]
  "Call a method on the server."
  (while True
    (try
      (await (handle-frames (await (.recv-multipart frontend))))
      (except [zmq.Again])
      (except [KeyboardInterrupt]
        (log.info f"Interrupted, closing {n}")
        (break))
      (except [err [Exception]]
        (log.error f"server-loop {n} error:" :exception err)))))

(defn :async background-loop []
  "Background service tasks."
  ;; TODO set come sort of server status here
  (print "Initial map generation...")
  (await (engine.init))
  (print "Ready for players.")
  ;; TODO set server status to ready
  (while True
    (await (asyncio.sleep BACKGROUND_TICK))
    (try
      (await (engine.extend-world))
      (await (engine.develop))
      (await (engine.spawn-characters))
      (await (engine.spawn-items))
      (engine.set-offline-players)
      (except [err [Exception]]
        (log.error "background-loop exception" :exception err)))))

(defn :async serve []
  (print f"Starting server at {(.isoformat (datetime.today))} for {config-file}")
  (log.info f"Starting server for {config-file}")
  (let [tasks (lfor n (range N_CONCURRENT_CLIENTS) (asyncio.create-task (server-loop n)))
        bg-task (asyncio.create-task (background-loop))]
    (try
      ; TODO: clean exit
      ;(setv [done pending] (await (asyncio.wait [bg-task #* tasks])))
      (await (asyncio.wait [bg-task #* tasks]))
      #_(except [KeyboardInterrupt]
          (print "Interrupted, quitting.")
          (log.info f"Interrupted, quitting")
          (for [t pending]
             (t.cancel))))))
