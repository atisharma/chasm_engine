"
Functions that deal with recall and vector databases.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import os)
(import pathlib [Path])

(import openai [Embedding])
(import tenacity [retry stop-after-attempt wait-random-exponential])

(import chromadb)
(import chromadb.config [Settings])
(import chromadb.utils.embedding-functions [OpenAIEmbeddingFunction
                                            SentenceTransformerEmbeddingFunction])

(import chasm_engine [log])

(import chasm_engine.stdlib *)


;;; -----------------------------------------------------------------------------

(defclass MemoryError [Exception])

(setv path (config "world"))

(defn chroma [vdb-path]
  "Return the chroma client."
  (chromadb.PersistentClient
    :path vdb-path
    :settings (Settings :anonymized-telemetry False)))

;; chromadb is a singleton,
;; which sadly means no concurrency and no writing from child threads
(setv _vdb (chroma (.join "/" [path "memory"])))

(defn get-embedding-fn []
  "Return the configured embedding function."
  (let [provider (config "memory" "embedding_provider")]
    (if provider
        (let [params (config "providers" provider)
              model (or (config "memory" "embedding") "text-embedding-ada-002")]
          (OpenAIEmbeddingFunction :model-name model
                                   :api-key (:api-key params "N/A")
                                   :api-base (:api-base params None)
                                   :api-type (:api-type params None)
                                   :organization-id (:organization-id params None))) 
        (let [model (or (config "memory" "embedding") "all-MiniLM-L6-v2")]
          (SentenceTransformerEmbeddingFunction :model-name model)))))

(defn collection [name]
  (log.info f"memory/collection {name}")
  (_vdb.get-or-create-collection :name f"C-{name}"
                                 :embedding-function (get-embedding-fn)))

(defn [(retry :wait (wait-random-exponential :min 0.5 :max 30)
              :stop (stop-after-attempt 6))] remote-embed [text #** kwargs]
  "Get an embedding from text via the API."
  (let [params (config "providers" (config "memory" "embedding_provider"))
        model {"model" (or (config "memory" "embedding") "all-mpnet-base-v2")}
        response (Embedding.create :input text
                                   #** (| params kwargs model))]
    (-> response
        (:data)
        (first)
        (:embedding))))

(defn peek [name]
  (.peek (collection name)))

;;; -----------------------------------------------------------------------------

(defn add [name metadata text]
  "Add text, metadata and vector embedding to the corresponding index."
  (let [vdbc (collection name)]
    (vdbc.add :documents text
              :metadatas metadata
              :ids (hash-id text))))

(defn query [name text [n 6] [where None]]
  "Recall related memories."
  (let [vdbc (collection name)]
    ; can use where field to filter on metadata
    (vdbc.query :query-texts [text]
                :n-results n
                :where where)))
      
(defn recent [name [n 6] [where None]]
  (let [c (collection name)
        ct (.count c)]
    (.get c
          :limit (min n ct)
          :offset (- ct (min n ct))
          :where where)))
    
