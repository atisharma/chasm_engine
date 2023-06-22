"
Chat management functions.
"
(require hyrule.argmove [-> ->>])

(import functools [partial])

(import openai [ChatCompletion])

(import .stdlib *)


;;; -----------------------------------------------------------------------------
;;; Message functions
;;; -----------------------------------------------------------------------------

(defn msg [role #* content]
  "Just a simple dict with the needed fields."
  {"role" role
   "content" (->> content
                  (.join "\n")
                  (.replace "</s>" "")
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

(defn prepend [x #^list l]
  "Prepend x at the front of list l."
  (+ [x] l))

(defn append [x #^list l]
  "Prepend x at the front of list l."
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
       (filter None)
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

(defn respond [messages #** kwargs]
  "Reply to a list of messages and return just content.
The messages should already have the standard roles."
  (let [response (ChatCompletion.create
                   :messages messages
                   #** (| (config "OpenAI") kwargs))]
    (-> response.choices
        (first)
        (:message)
        (:content))))

(defn chat [messages]
  "Add an assistant response to a list of messages.
The messages should already have the standard roles."
  (-> (respond messages)
      (assistant)
      (append messages)))

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

(defn true-false [messages context query]
  "Respond with Boolean to a query."
  (let [response (respond [(system "You are a helpful assistant who follows instructions carefully. You reply to the query with either 'True' or 'False' as best you can based on the truth of the query.")
                           #* messages
                           (user f"The query is:
{query}

Consider the following additional context in your evaluation:
{context}

Respond with only one boolean, either 'True' or 'False'.")
                           (assistant "My single-word boolean response is:")])]
    (or (similar response "True")
        (in "true" (.lower response)))))

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

