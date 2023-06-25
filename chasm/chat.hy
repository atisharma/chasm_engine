"
Chat management functions.
"
(require hyrule.argmove [-> ->>])

(import chasm [log])

(import functools [partial])

(import openai [ChatCompletion Edit])
(import tiktoken)

(import chasm.stdlib *)


;;; -----------------------------------------------------------------------------

(defn token-length [x]
  "The number of tokens, roughly, of a chat history (or anything with a meaningful __repr__).
We use tiktoken because I don't want to install pytorch."
  (let [encoding (tiktoken.get-encoding "cl100k_base")]
    (->> x
         (str)
         (encoding.encode)
         (len))))

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

(defn truncate [messages [length 1500]]
  "Hack until below length."
  (while (> (token-length messages) length)
    (setv messages (cut messages 2 None)))
  messages)

(defn prepend [x #^list l]
  "Prepend x at the front of list l."
  (+ [x] l))

(defn append [x #^list l]
  "Append x to list l."
  (+ l [x]))

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
  "Follow an instruction.
`input`: The input text to use as a starting point for the edit.
`instruction`: how the model should edit the prompt."
  (let [params (config "OpenAI")
        chat-model (.pop params "chat_model")
        completion-model (.pop params "completion_model")
        response (Edit.create
                   :input text
                   :instruction instruction
                   :model completion-model
                   #** (| params kwargs))]
    (-> response.choices
        (first)
        (:text))))

(defn respond [messages #** kwargs]
  "Reply to a list of messages and return just content.
The messages should already have the standard roles."
  (let [params (config "OpenAI")
        chat-model (.pop params "chat_model")
        completion-model (.pop params "completion_model")
        response (ChatCompletion.create
                   :messages messages
                   :model chat-model
                   #** (| params kwargs))]
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

;;; -----------------------------------------------------------------------------
;;; Prompts over paragraphs of text -> text
;;; -----------------------------------------------------------------------------

(defn text->topic [text]
  "Create a topic summary from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"Please express the topic of the following text in as few words as possible:

{(text)}")
            (assistant "The topic is as follows:")]))

(defn text->points [text]
  "Create bullet points from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user "Summarize the following text as a list of bullet points, preserving the most interesting, pertinent and important points.

{(text)}")
            (assistant "The points are as follows:")]))

(defn text->summary [text]
  "Create short summary from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"Please concisely rewrite the following text, preserving the most interesting, pertinent and important points.

{(text)}")
            (assistant "The summary is as follows:")]))

(defn text->extract [query text]
  "Extract points relevant to a query from text."
  (respond [(system "You are a helpful assistant who follows instructions carefully.")
            (user f"{(query)}

Please concisely rewrite the following text, extracting the points most interesting, pertinent and important to the preceding query. Don't invent information. If there is no relevant information, be silent.

{(text)}")
            (assistant "The points are as follows:")]))

;;; -----------------------------------------------------------------------------
;;; Combined text and messages -> text
;;; -----------------------------------------------------------------------------

