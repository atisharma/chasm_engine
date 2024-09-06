"
Chat management functions.
"
(require hyrule.argmove [-> ->>])

(import chasm_engine [log])

(import tiktoken)
(import openai [AsyncOpenAI])
(import openai [APIConnectionError APIError OpenAIError])
(import replicate)

(import tenacity [retry stop-after-attempt wait-random-exponential])

(import chasm_engine.stdlib *)

;;; -----------------------------------------------------------------------------

(defclass ChatError [Exception])

;;; -----------------------------------------------------------------------------
;;; Message functions
;;; -----------------------------------------------------------------------------

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

;;; -----------------------------------------------------------------------------
;;; Chat functions
;;; -----------------------------------------------------------------------------

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
This will fail if the system messages add up to be too long.
Non-destructive."
  (let [l (- (config "context_length") (or spare-length (config "max_tokens") 300))
        ms (.copy messages)
        roles (set (map (fn [x] (:role x)) ms))
        too-long (> (token-length (str messages)) l)]
    (cond (and too-long
               (= (len roles) 1)) (raise (ChatError f"System messages too long ({(token-length (str ms))} tkns) - nothing left to cut."))
          too-long (do (for [m ms]
                         (when (!= (:role m) "system")
                           (.remove ms m)
                           (break)))
                       (truncate ms :spare-length spare-length))
          :else messages)))

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
  (+ (.join "\n"
            (lfor m messages
                  (let [role (:role m)
                        content (:content m)]
                    (match role
                           "system" f"{system-tag}{content}{system-close-tag}"
                           "assistant" f"{assistant-tag}{content}{assistant-close-tag}"
                           "user" f"{user-tag}{content}{user-close-tag}"))))
     f"\n{assistant-tag}"))
    

;;; -----------------------------------------------------------------------------
;;; Remote API calls
;;; -----------------------------------------------------------------------------

(defn/a edit [text instruction #** kwargs]
  "Follow an instruction. Now uses the chat endpoint because the edit one is to be deprecated.
`text`: The input text to use as a starting point for the edit.
`instruction`: how the model should edit the prompt."
  (-> [(system instruction)
       (user text)]
      (respond #** kwargs)
      (await)))

(defn/a _openai [params messages]
  "Openai-compatible API calls: https://platform.openai.com/docs/api-reference"
  (let [client (AsyncOpenAI :api-key (.pop params "api_key")
                            :base_url (.pop params "api_base"))
        response (await
                   (client.chat.completions.create
                     :messages (standard-roles messages)
                     #** params))]
    (. (. (first response.choices) message) content)))

(defn/a _replicate [params messages]
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

(defn/a [(retry :wait (wait-random-exponential :min 0.5 :max 30) :stop (stop-after-attempt 6))]
  respond [messages #** kwargs]
  "Reply to a list of messages and return just content.
The messages should already have the standard roles."
  (let [providers (list (.keys (config "providers")))
        provider (choice providers)
        conf (config "providers" provider)
        defaults {"api_key" "n/a"
                  "max_tokens" (config "max_tokens")
                  "model" "gpt-3.5-turbo"
                  "api_scheme" "openai"}
        params (| defaults conf kwargs)
        api-scheme (.pop params "api_scheme")]
    (match api-scheme
           "replicate" (await (_replicate params messages))
           "openai"    (await (_openai params messages))
           _           (await (_openai params messages)))))

(defn/a chat [messages #** kwargs] ; -> message
  "An assistant response (message) to a list of messages.
The messages should already have the standard roles."
  (-> (respond messages #** kwargs)
      (await)
      (assistant)))

;;; -----------------------------------------------------------------------------
;;; Prompts over messages -> text
;;; -----------------------------------------------------------------------------

(defn/a msgs->topic [messages]
  "Create a topic summary from messages."
  (await (respond [(system "Your sole purpose is to express the topic of conversation in one short sentence.")
                   #* messages
                   (user "Summarize the topic of conversation before now in as few words as possible.")
                   (assistant "The topic is as follows:")])))

(defn/a msgs->points [messages]
  "Create bullet points from messages."
  (await (respond [(system "Your sole purpose is to summarize the conversation into bullet points.")
                   #* messages
                   (user "Summarize this conversation before now as a markdown list, preserving the most interesting, pertinent and important points.")
                   (assistant "The main points are as follows:")])))

(defn/a msgs->summary [messages]
  "Create summary from messages."
  (await (respond [(system "You are a helpful assistant who follows instructions carefully.")
                   #* messages
                   (user "Please edit down the conversation before now into a single concise paragraph, preserving the most interesting, pertinent and important points.")
                   (assistant "The summary is as follows:")])))

(defn/a text&msgs->reply [messages context query]
  "Respond in the context of messages and text.
The text should not be so long as to cause context length problems, so summarise it first if necessary."
  (await (respond [(system "You are a helpful assistant who follows instructions carefully.")
                   #* messages
                   (user f"{query}

Consider the following additional context before responding:
{context}")])))

(defn/a yes-no [messages context query]
  "Respond with yes or no to a query."
  (let [response (await (respond [(system "Reply to the query with either 'yes' or 'no' as best you can based on the context and conversation.
Below is the conversation or story.")
                                  #* messages
                                  (user f"The query to evaluate is:
'{query}'

Consider the following important context in your evaluation:
{context}

Respond with only one word, either 'yes' or 'no'.")
                                  (assistant "My single-word yes/no response is:")]))]
    (or (similar response "yes")
        (in "yes" (.lower response)))))

(defn/a complete-json [template instruction context [max-tokens 600]]
  "Fill in a JSON template according to context. Return list, dict or None.
JSON completion is a bit unreliable, depending on the model."
  (let [messages [(system (.join "\n"
                              ["You will be given a JSON template to complete. You must stick very closely to the format of the template."
                               instruction]))
                  (user (.join "\n\n"
                               [context
                                "Below is the JSON template to complete."
                                template
                                "Now, complete the template. Give only valid JSON, no other text, context or explanation."]))]
        response (await (respond messages :max-tokens max-tokens))
        match (re.search r"[^{\[]*([\[{].*[}\]])" response :flags re.S)]
    (try
      (if match
          (-> match
              (.groups)
              (first)
              (json.loads))
          (log.error f"bad JSON creation, can't match:\n{response}"))
      (except [json.decoder.JSONDecodeError]
        (log.error f"bad JSON creation, can't decode:\n{response}")))))

(defn/a complete-lines [template instruction context attributes [max-tokens 600]]
  "Fill in a template according to context, one per line. Return dict or None.
Provided `attributes` should be a list of strings.
Format is as
`attribute_1: value
attribute_2: value`"
  (let [messages [(system (.join "\n\n"
                                 ["You will be given a template to complete. You must stick very closely to the format of the template.
Give one attribute per line, no commentary, examples or other notes, just the template with the attribute values updated."
                                  instruction]))
                  (user (.join "\n\n"
                               [context
                                "Below is the template to complete."
                                template
                                "Now, complete the template, one attribute per line. Give only new values, no other text or explanation."]))]
        response (await (respond messages :max-tokens max-tokens))]
    (grep-attributes response attributes)))

;;; -----------------------------------------------------------------------------
;;; Prompts over paragraphs of text -> text
;;; -----------------------------------------------------------------------------

(defn/a text->topic [text]
  "Create a topic summary from text."
  (await (respond [(system "You are a helpful assistant who follows instructions carefully.")
                   (user f"Please express the topic of the following text in as few words as possible:

{text}")
                   (assistant "The topic is as follows:")])))

(defn/a text->points [text]
  "Create bullet points from text."
  (await (respond [(system "You are a helpful assistant who follows instructions carefully.")
                   (user f"Summarize the following text as a list of bullet points, preserving the most interesting, pertinent and important points.

{text}")
                   (assistant "The points are as follows:")])))

(defn/a text->summary [text]
  "Create short summary from text."
  (await (respond [(system "You are a helpful assistant who follows instructions carefully.")
                   (user f"Please concisely rewrite the following text, preserving the most interesting, pertinent and important points.

{text}")
                   (assistant "The summary is as follows:")])))

(defn/a text->extract [query text]
  "Extract points relevant to a query from text."
  (await (respond [(system "You are a helpful assistant who follows instructions carefully.")
                   (user f"{query}

Please concisely rewrite the following text, extracting the points most interesting, pertinent and important to the preceding query. Don't invent information. If there is no relevant information, be silent.

{text}")
                   (assistant "The points are as follows:")])))

;;; -----------------------------------------------------------------------------
;;; Combined text and messages -> text
;;; -----------------------------------------------------------------------------
