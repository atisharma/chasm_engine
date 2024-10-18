"
Chat management functions.
"

(require hyrule.argmove [-> ->>])
(require hyjinx.macros [prepend append])

(import hyjinx [first])

(import chasm-engine [log])

(import tiktoken)
(import openai)
(import anthropic)

(import tenacity [retry retry-if-exception-type stop-after-attempt wait-random-exponential])

(import chasm_engine.lib *)


(defclass ChatError [RuntimeError])

(setv APIErrors [openai.APIConnectionError
                 openai.InternalServerError
                 openai.APIStatusError
                 openai.APITimeoutError
                 anthropic.APIConnectionError
                 anthropic.InternalServerError
                 anthropic.APIStatusError
                 anthropic.APITimeoutError])

;; Message functions
;; -----------------------------------------------------------------------------

(defn msg [role content]
  "Just a simple dict with the needed fields."
  (if content
      {"role" role
       "content" (.strip content)}
      (raise (ChatError f"No content in message (role: {role})."))))

(defn system [content]
  (msg "system" content))

(defn user [content]
  (msg "user" content))

(defn assistant [content]
  (msg "assistant" content))

;; Chat functions
;; -----------------------------------------------------------------------------

(defn token-length [x]
  "The number of tokens, roughly, of a chat history (or anything with a meaningful __repr__).
  We use tiktoken because I don't want to install pytorch."
  (let [encoding (tiktoken.get-encoding "cl100k_base")]
    (->> x
         (str)
         (encoding.encode)
         (len))))

(defn standard-roles [messages * [roles ["assistant" "user" "system"]]]
  "Remove messages not with standard role."
  (lfor m messages
        :if (in (:role m) roles)
        m))
  
(defn truncate [messages [spare-length None]]
  "Hack away non-system messages until below length.
  This will fail if the system message is too long.
  Non-destructive."
  (let [l (- (config "context_length") (or spare-length (config "max_tokens") 300))
        ms (.copy messages)
        roles (set (map (fn [x] (:role x)) ms))
        too-long (> (token-length (str messages)) l)]
    (cond
      (and too-long (= (len roles) 1))
      (raise (ChatError f"System messages too long ({(token-length (str ms))} tkns) - nothing left to cut."))

      too-long
      (do (for [m ms]
            ;; remove the first non-system message
            (when (!= (:role m) "system")
              (.remove ms m)
              (break)))
          (truncate ms :spare-length spare-length))

      ;; first non-system message must be a user message
      (and (= "system" (:role (first messages)))
           (= "assistant" (:role (second messages))))
      (+ (first messages) (cut messages 2 None))

      :else
      messages)))

(defn msg->dlg [user-name assistant-name message]
  "Replace standard roles with given names and ignore roles with system messages.
  Return modified dialogue message or None." 
  (let [role (:role message)]
    (cond (= role "user") (msg user-name (:content message))
          (= role "assistant") (msg assistant-name (:content message))
          :else None)))

(defn msgs->dlg [user-name assistant-name messages]
  "Replace standard roles with given names and filter out system messages.
  Return dialogue." 
  (->> messages
       (map (partial msg->dlg user-name assistant-name))
       (sieve)
       (list)))

(defn dlg->msg [user-name assistant-name message]
  "Replace given names with standard roles and replace other roles with system messages.
  Return modified message."
  (let [role (:role message)]
    (cond (= role user-name) (user (:content message))
          (= role assistant-name) (assistant (:content message))
          :else (system f"{role}: {(:content message)}"))))
    
(defn dlg->msgs [user-name assistant-name messages]
  "Replace given names with standard roles and replace other roles with system messages.
  Return modified messages."
  (->> messages
       (map (partial dlg->msg user-name assistant-name))
       (list)))

(defn flip-roles [messages]
  (dlg->msgs "assistant" "user" messages))

;; Remote API calls
;; -----------------------------------------------------------------------------

;; TODO hyjinx llm async
;; TODO streaming -- but how to stream to client?

(defn :async _openai [params messages]
  "Openai-compatible API calls: https://platform.openai.com/docs/api-reference"
  (let [client (openai.AsyncOpenAI :api-key (.pop params "api_key")
                                   :base-url (.pop params "api_base"))
        response (await
                   (client.chat.completions.create
                     :messages (standard-roles messages)
                     #** params))]
    (. (. (first response.choices) message) content)))

(defn :async _anthropic [params messages]
  "Use the Anthropic API.
  The Anthropic Messages API requires max-tokens.
  It also ccepts a top-level `system` parameter, not \"system\"
  as an input message role."
  (let [client (anthropic.AsyncAnthropic :api-key (.pop params "api_key"))
        system-prompt (jn (lfor m messages
                            :if (= (:role m) "system")
                            (:content m)))
        max-tokens (.pop params "max_tokens" 500) ; requires a default max-tokens
        response (await
                   (client.messages.create
                     :system system-prompt
                     :messages (standard-roles messages :roles ["user" "assistant"])
                     :max-tokens max-tokens
                     #** params))]
    (. (first response.content) text)))

(defn :async 
  [(retry :wait (wait-random-exponential :min 0.5 :max 10)
          :stop (stop-after-attempt 6)
          :retry (retry-if-exception-type APIErrors))]
  respond [messages [provider "backend"] #** kwargs]
  "Reply to a list of messages and return just content.
  The messages should already have the standard roles.
  Use `providers.default` unless the `provider` arg is specified."
  (let [conf (config "providers" provider)
        defaults {"api_key" "n/a"
                  "max_tokens" (config "max_tokens")
                  "api_scheme" "openai"
                  "model" None}
        params (| defaults conf kwargs)
        api-scheme (.pop params "api_scheme")]
    (try
      (match api-scheme
             "openai"    (await (_openai params messages))
             "anthropic" (await (_anthropic params messages))
             _           (await (_openai params messages)))
      (except [err [Exception]]
        (log.error f"Chat API exception ({api-scheme})" :exception err)
        (raise (ChatError (.join " " [api-scheme (str err)])))))))

(defn :async chat [messages #** kwargs] ; -> message
  "An assistant response (message) to a list of messages.
  The messages should already have the standard roles."
  (-> (respond messages #** kwargs)
      (await)
      (assistant)))
