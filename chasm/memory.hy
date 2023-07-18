"
Functions that deal with recall and vector databases.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import os)
(import pathlib [Path])

(import chromadb)
(import chromadb.config [Settings])
(import chromadb.utils.embedding-functions [OpenAIEmbeddingFunction])

(import openai [Embedding])

(import chasm [log])

(import chasm.stdlib *)


;;; -----------------------------------------------------------------------------

(defclass MemoryError [Exception])

(setv path (config "world"))

(defn chroma [vdb-path]
  "Return the chroma client."
  (chromadb.PersistClient
    :path vdb-path
    :settings (Settings :anonymized-telemetry False)))

;; chromadb is a singleton,
;; which sadly means no concurrency and no writing from child threads
(setv _vdb (chroma (.join "/" [path "memory"])))

(defn collection [name]
  (let [params (config "providers" (config "memory" "embedding_provider"))
        model (or (config "memory" "embedding") "text-embedding-ada-002")
        ef (OpenAIEmbeddingFunction :model-name model
                                    :api-key (:api-key params "N/A")
                                    :api-base (:api-base params None)
                                    :api-type (:api-type params None)
                                    :organization-id (:organization-id params None))]
    (log.info f"memory/collection {name}")
    (_vdb.get-or-create-collection :name f"C-{name}"
                                  :embedding-function ef)))

(defn embed [text #** kwargs]
  "Get an embedding from text via the API."
  (let [params (config "providers" (config "memory" "embedding_provider"))
        model {"model" (or (config "memory" "embedding")
                           "text-embedding-ada-002")}
        response (Embedding.create :input text
                                   #** (| params kwargs model))]
    (-> response
        (:data)
        (first)
        (:embedding))))

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
    (vdbc.query :query-embeddings [(embed text)]
                :n-results n
                :where where)))
      
(defn peek [name]
  (.peek (collection name)))

(defn recent [name [n 6] [where None]]
  (let [c (collection name)
        ct (.count c)]
    (.get c
          :offset (- ct (min n ct))
          :where where)))
    
