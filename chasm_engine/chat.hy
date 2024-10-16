"
Chat management functions.
"

(require hyrule.argmove [-> ->>])
(require hyjinx.macros [prepend append])
(require chasm-engine.instructions [deftemplate])

(import hyjinx [first])

(import chasm-engine [log])

(import tiktoken)
(import openai [AsyncOpenAI])
(import openai [InternalServerError APIStatusError APITimeoutError])
(import replicate)
(import anthropic [AsyncAnthropic])

(import tenacity [retry retry-if-exception-type stop-after-attempt wait-random-exponential])

(import chasm_engine.lib *)


(defclass ChatError [RuntimeError])

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

(defn standard-roles [messages]
  "Remove messages not with standard role."
  (lfor m messages
        :if (in (:role m) ["assistant" "user" "system"])
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

(defn llama-format [messages
                    [system-tag "SYSTEM:"] [assistant-tag "ASSISTANT:"] [user-tag "USER:"]
                    [system-close-tag ""] [assistant-close-tag ""] [user-close-tag ""]]
  "Format standard role messages to Llama 2 tags. Remove system prompts."
  ;; this is for use with Replicate.
  (+ (.join "\n"
            (lfor m messages
                  (let [role (:role m)
                        content (:content m)]
                    (match role
                           "system" f"{system-tag}{content}{system-close-tag}"
                           "assistant" f"{assistant-tag}{content}{assistant-close-tag}"
                           "user" f"{user-tag}{content}{user-close-tag}"))))
     f"\n{assistant-tag}"))
    

;; Remote API calls
;; -----------------------------------------------------------------------------

;; TODO hyjinx llm async
;; TODO streaming -- but how to stream to client?

(defn :async _openai [params messages]
  "Openai-compatible API calls: https://platform.openai.com/docs/api-reference"
  (let [client (AsyncOpenAI :api-key (.pop params "api_key")
                            :base-url (.pop params "api_base"))
        response (await
                   (client.chat.completions.create
                     :messages (standard-roles messages)
                     #** params))]
    (. (. (first response.choices) message) content)))

(defn :async _replicate [params messages]
  "Replicate-compatible API calls: https://replicate.com/docs"
  (.pop params "api_key" None)
  ;(log.info f"params {params}") 
  (let [api-token (.pop params "api_token" None)
        model (.pop params "model")
        system-tag (.pop params "system_tag" None)
        assistant-tag (.pop params "assistant_tag" None)
        user-tag (.pop params "user_tag" None)
        system-close-tag (.pop params "system_close_tag" None)
        assistant-close-tag (.pop params "assistant_close_tag" None)
        user-close-tag (.pop params "user_close_tag" None)
        client (replicate.client.Client :api-token api-token)
        response (await (client.async_run
                          model
                          :stream False
                          :input {"prompt" (llama-format (standard-roles messages)
                                                         :system-tag system-tag :assistant-tag assistant-tag :user-tag user-tag 
                                                         :system-close-tag system-close-tag :assistant-close-tag assistant-close-tag :user-close-tag user-close-tag) 
                                  #** params}))]
    ; async client sometimes returns an iterator, sometimes not, but we are not streaming (yet)
    (if (isinstance response str)
        response
        (.join "" response))))

(defn :async _anthropic [params messages]
  "Use the Anthropic API."
  (let [client (AsyncAnthropic :api-key (.pop params "api_key"))
        response (await
                   (client.messages.create
                     :messages (standard-roles messages)
                     #** params))]
    (. (first response.content) text)))

(defn :async 
  [(retry :wait (wait-random-exponential :min 0.5 :max 10)
          :stop (stop-after-attempt 6)
          :retry (retry-if-exception-type [InternalServerError APIStatusError APITimeoutError]))]
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
    (match api-scheme
           "openai"    (await (_openai params messages))
           "replicate" (await (_replicate params messages))
           "anthropic" (await (_anthropic params messages))
           _           (await (_openai params messages)))))

(defn :async chat [messages #** kwargs] ; -> message
  "An assistant response (message) to a list of messages.
  The messages should already have the standard roles."
  (-> (respond messages #** kwargs)
      (await)
      (assistant)))
