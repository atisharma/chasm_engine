"
Chat management functions.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import openai [ChatCompletion])
(import tiktoken)

(import chasm.stdlib *)
(import chasm [state])


;;; -----------------------------------------------------------------------------

(defclass ChatError [Exception])

;;; -----------------------------------------------------------------------------
;;; Message functions
;;; -----------------------------------------------------------------------------

(defn msg [role #* content]
  "Just a simple dict with the needed fields."
  {"role" role
   "content" (->> content
                  (.join "\n")
                  (.strip))})

(defn system [#* content]
  (msg "system" #* content))

(defn user [#* content]
  (msg "user" #* content))

(defn assistant [#* content]
  (msg "assistant" #* content))

;;; -----------------------------------------------------------------------------
;;; Chat functions
;;; -----------------------------------------------------------------------------

; TODO: extract and store events of chopped bit

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
    
(defn dlg->msgs  [user-name assistant-name messages]
  "Replace given names with standard roles and replace other roles with system messages.
Return modified messages."
  (->> messages
       (map (partial dlg->msg user-name assistant-name))
       (list)))

(defn flip-roles [messages]
  (dlg->msgs "assistant" "user" messages))

;;; -----------------------------------------------------------------------------
;;; Remote API calls
;;; -----------------------------------------------------------------------------

(defn edit [text instruction #** kwargs]
  "Follow an instruction. Now uses the chat endpoint because the edit one is to be deprecated.
`text`: The input text to use as a starting point for the edit.
`instruction`: how the model should edit the prompt."
  (respond [(system instruction)
            (user text)]))

(defn respond [messages #** kwargs]
  "Reply to a list of messages and return just content.
The messages should already have the standard roles."
  (let [params (config "providers" (config "provider"))
        defaults {"max_tokens" (config "max_tokens")
                  "model" "gpt-3.5-turbo"}
        response (ChatCompletion.create
                   :messages (standard-roles messages)
                   #** (| defaults params kwargs))]
    (-> response.choices
        (first)
        (:message)
        (:content))))

(defn chat [messages #** kwargs] ; -> message
  "An assistant response (message) to a list of messages.
The messages should already have the standard roles."
  (-> (respond messages #** kwargs)
      (assistant)))

;;; -----------------------------------------------------------------------------
;;; Prompts over messages -> text
;;; -----------------------------------------------------------------------------

(defn msgs->topic [messages]
  "Create a topic summary from messages."
  (respond [(system "Your sole purpose is to express the topic of conversation in one short sentence.")
            #* messages
            (user "Summarize the topic of conversation before now in as few words as possible.")
            (assistant "The topic is as follows:")]))

(defn msgs->points [messages]
  "Create bullet points from messages."
  (respond [(system "Your sole purpose is to summarize the conversation into bullet points.")
            #* messages
            (user "Summarize this conversation before now as a markdown list, preserving the most interesting, pertinent and important points.")
            (assistant "The main points are as follows:")]))

(defn msgs->summary [messages]
  "Create summary from messages."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            #* messages
            (user "Please edit down the conversation before now into a single concise paragraph, preserving the most interesting, pertinent and important points.")
            (assistant "The summary is as follows:")]))

(defn text&msgs->reply [messages context query]
  "Respond in the context of messages and text.
The text should not be so long as to cause context length problems, so summarise it first if necessary."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            #* messages
            (user f"{query}

Consider the following additional context before responding:
{context}")]))

(defn yes-no [messages context query]
  "Respond with yes or no to a query."
  (let [response (respond [(system "Reply to the query with either 'yes' or 'no' as best you can based on the context and conversation.
Below is the conversation or story.")
                           #* messages
                           (user f"The query to evaluate is:
'{query}'

Consider the following important context in your evaluation:
{context}

Respond with only one word, either 'yes' or 'no'.")
                           (assistant "My single-word yes/no response is:")])]
    (or (similar response "yes")
        (in "yes" (.lower response)))))

(defn complete-json [template instruction context [max-tokens 600]]
  "Fill in a JSON template according to context. Return list, dict or None.
JSON completion is a bit unreliable, depending on the model."
  (let [messages [(system "You will be given a JSON template to complete. You must stick very closely to the format of the template.")
                  (system instruction)
                  (user context)
                  (system "Below is the JSON template to complete.")
                  (user template)
                  (system "Now, complete the template. Give only valid JSON, no other text, context or explanation.")]
        response (respond messages :max-tokens max-tokens)
        match (re.search r"[^{\[]*([\[{].*[}\]])" response :flags re.S)]
    (try
      (if match
          (-> match
              (.groups)
              (first)
              (json.loads))
          (log.error f"chat/complete-json: bad JSON creation, can't match:\n{response}"))
      (except [json.decoder.JSONDecodeError]
        (log.error f"chat/complete-json: bad JSON creation, can't decode:\n{response}")))))

(defn complete-lines [template instruction context attributes [max-tokens 600]]
  "Fill in a template according to context, one per line. Return dict or None.
Provided `attributes` should be a list of strings.
Format is as
`attribute_1: value
attribute_2: value`"
  (let [messages [(system "You will be given a template to complete. You must stick very closely to the format of the template.
Give one attribute per line, no commentary, examples or other notes, just the template with the attribute values updated.")
                  (system instruction)
                  (user context)
                  (system "Below is the template to complete.")
                  (user template)
                  (system "Now, complete the template. Give only new values, no other text or explanation.")]
        response (respond messages :max-tokens max-tokens)]
    (grep-attributes response attributes)))

;;; -----------------------------------------------------------------------------
;;; Prompts over paragraphs of text -> text
;;; -----------------------------------------------------------------------------

(defn text->topic [text]
  "Create a topic summary from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"Please express the topic of the following text in as few words as possible:

{text}")
            (assistant "The topic is as follows:")]))

(defn text->points [text]
  "Create bullet points from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"Summarize the following text as a list of bullet points, preserving the most interesting, pertinent and important points.

{text}")
            (assistant "The points are as follows:")]))

(defn text->summary [text]
  "Create short summary from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"Please concisely rewrite the following text, preserving the most interesting, pertinent and important points.

{text}")
            (assistant "The summary is as follows:")]))

(defn text->extract [query text]
  "Extract points relevant to a query from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"{query}

Please concisely rewrite the following text, extracting the points most interesting, pertinent and important to the preceding query. Don't invent information. If there is no relevant information, be silent.

{text}")
            (assistant "The points are as follows:")]))

;;; -----------------------------------------------------------------------------
;;; Combined text and messages -> text
;;; -----------------------------------------------------------------------------
