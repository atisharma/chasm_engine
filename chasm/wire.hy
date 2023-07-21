"
The server protocol implementation.
The signature verification happens in server.hy, not here, because it involves looking up the public key.
"
(import json)
(import zmq)
(import time [time])
(import uuid [uuid1])

(setv sender-id (. (uuid1) hex))


(setv CHASM_PROTOCOL_VERSION "0.0.1")
(setv CHASM_SERVER_VERSION "0.0.1")


(defn wrap [payload]
  "Format and wrap message."
  (json.dumps {"payload" payload
               "proto_version" CHASM_PROTOCOL_VERSION
               "server_version" CHASM_SERVER_VERSION
               "zmq_version" zmq.__version__
               "sender_id" sender-id
               "sender_time" (str (time))}))

(defn unwrap [zmsg]
  "Unwrap message. Return None if it doesn't decode. Otherwise, return function and data."
  (try (json.loads zmsg)
    (except [json.JSONDecodeError]
      (log.error f"wire/unwrap: {zmsg}"))))
    
(defn zerror [code message]
  {"error" {"code" code "message" message}})
