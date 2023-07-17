(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])
(import hyrule [inc dec rest butlast starmap distinct])

(import functools [partial cache lru-cache])
(import itertools *)

(import importlib)
(import os)
(import re)
(import json)
(import readline)
(import string [capwords])
(import pathlib [Path])
(import hashlib [sha1 pbkdf2-hmac])
(import hmac [compare-digest])
(import random [shuffle randint choice])

(import jaro)

;; tomllib for python 3.11 onwards
;; when we move to 3.11, we can remove this
(try
  (import tomllib)
  (except [ModuleNotFoundError]
    (import tomli :as tomllib)))


;;; -----------------------------------------------------------------------------
;;; meta functions
;;; -----------------------------------------------------------------------------

(defn mreload [#* modules]
  "Reload a whole list of modules."
  (for [m modules]
    (try (importlib.reload m)
         (except [e [ImportError]] 
            (print e)))))

;;; -----------------------------------------------------------------------------
;;; list functions
;;; -----------------------------------------------------------------------------

(defn first [xs]
  (next (iter xs)))

(defn last [xs]
  (when xs
    (next (reversed xs))))

(defn sieve [xs]
  (filter None xs))

(defn pairs [xs]
  "Split into pairs. So ABCD -> AB CD."
  (zip (islice xs 0 None 2) (islice xs 1 None 2)))
  
(defn prepend [x l]
  "Prepend x at the front of list l."
  (+ [x] l))

(defn append [x l]
  "Append x to list l."
  (+ l [x]))

;;; -----------------------------------------------------------------------------
;;; config functions
;;; -----------------------------------------------------------------------------

(setv config-file "server.toml")

(defn config [#* keys]
  "Get values in a toml file like a hashmap, but default to None."
  (unless (os.path.isfile config-file)
    (raise (FileNotFoundError config-file)))
  (try
    (-> config-file
        (slurp)
        (tomllib.loads)
        (get #* keys))
    (except [KeyError]
      None)))

;;; -----------------------------------------------------------------------------
;;; File & IO functions
;;; -----------------------------------------------------------------------------

(defn load [fname]
  "Read a json file. None if it doesn't exist."
  (let [path (Path fname)]
    (when (path.exists)
      (with [f (open fname
                     :mode "r"
                     :encoding "UTF-8")]
        (json.load f)))))

(defn save [obj fname]
  "Write an object as a json file."
  (with [f (open fname
                 :mode "w"
                 :encoding "UTF-8")]
    (json.dump obj f :indent 4)))

(defn file-append [record fname]
 "Append / write a dict to a file as json.
 If the file does not exist, initialise a file with the record.
 If the file exists, append to it.
 Cobbled together from https://stackoverflow.com/a/31224105
 it overwrites the closing ']' with the new record + a new ']'.
 POSIX expects a trailing newline."
  (when fname
    (if (os.path.isfile fname)
      (with [f (open fname :mode "r+" :encoding "UTF-8")]
        (.seek f 0 os.SEEK_END)
        (.seek f (- (.tell f) 2))
        (.write f (.format ",\n{}]\n" (json.dumps record :indent 4))))
      (with [f (open fname :mode "w" :encoding "UTF-8")]
        (.write f (.format "[\n{}]\n" (json.dumps record :indent 4)))))))

(defn slurp [fname]
  "Read a plain text file."
  (let [path (Path fname)]
    (when (path.exists)
      (path.read-text))))

(defn barf [text fname]
  "Write a plain text file."
  (with [f (open fname :mode "w" :encoding "UTF-8")]
    (.write f text)))

(defn mksubdir [d]
  (.mkdir (Path (.join "/" [path d]))
          :parents True
          :exist-ok True))  

;;; -----------------------------------------------------------------------------
;;; Hashing, id and password functions
;;; -----------------------------------------------------------------------------

(defn hash-id [s]
  "Hex digest of sha1 hash of string."
  (-> (s.encode "utf-8")
      (sha1)
      (.hexdigest)))

(defn short-id [x]
  "First 6 chars of hash-id."
  (cut (hash-id x) 6))

(defn hash-pw [pw]
  "Hash password with a secret salt."
  (let [salt (os.urandom 24)
        digest (pbkdf2-hmac "sha512"
                            (pw.encode "utf-8")
                            :iterations 100000
                            :salt salt)]
    {"salt" (.hex salt)
     "hexdigest" (.hex digest)}))

(defn check-pw [pw stored]
  "Check password is correct."
  (let [salt (bytes.fromhex (:salt stored))
        hexdigest (:hexdigest stored)]
    (compare-digest hexdigest (.hex (pbkdf2-hmac "sha512"
                                                 (pw.encode "utf-8")
                                                 :iterations 100000
                                                 :salt salt)))))

;;; -----------------------------------------------------------------------------
;;; random things
;;; -----------------------------------------------------------------------------

(defn dice [n]
  "True 1/n of the time."
  (not (randint 0 (- n 1))))

;;; -----------------------------------------------------------------------------
;;; String functions
;;; -----------------------------------------------------------------------------

(defn sstrip [s]
  "Strip surrounding whitespace, quotes, '.',
force to lowercase, remove 'the' from start of line."
  (re.sub r"^[tT]he " ""
          (-> s
              (.strip "\n\t .\"'`")
              (.lower))))

(defn debullet [markdown-list] ; -> str
  "Get the main items from a markdown list."
  (->> markdown-list
       (re.sub r"^[\*\-\.(\[ \])\d]*" "" :flags re.M) ; remove bullet points
       (re.sub r"^([\w ']*\w).*$" r"\1" :flags re.M))) ; get main item

(defn bullet [l]
  "Make a markdown bulleted list from l."
  (.join "\n" (lfor x l f"- {x}")))

(defn close-quotes [s]
  "
  If there is an odd number of quotes in a line, close the quote.
  "
  (.join "\n"
    (lfor line (-> s (.replace "\"\"" "\"") (.splitlines))
          (if (% (.count line "\"") 2)  ; if an odd number of quotes
            (if (= (get line -1) "\"")  ; if ends in a quote
              (+ "\"" line)             ; close at start
              (+ line "\""))            ; close at end
            line))))

(defn trim-prose [s]
  "Remove any incomplete sentence."
  ; TODO: test handling of quotes and dialogue
  (let [paras (-> s
                  (.strip)
                  (->> (re.sub r"\n{3,}" r"\n\n" :flags re.M))
                  ;(.replace "\n\n\n" "\n\n")
                  (.split "\n\n")
                  (->> (filter (fn [x] (> (len x) 3)))
                       (map (fn [x] (when x (.strip x "\n\t")))))
                  (sieve)
                  (list))
        text (.join "\n\n" paras)
        m (re.match r"(.*[.?!*\"])[^.?!*\"]+" text :flags re.S)]
    (when m
      (first (m.groups)))))
  
(defn just-text [s]
  "Remove non-word and non-punctuation characters."
  (re.sub r"[^\w&-,.!?']+" "" s))
  
(defn last-word? [s1 s2]
  "Is one string the last word of the other?"
  (let [ss1 (.split s1)
        ss2 (.split s2)])
  (or (and s2 (= s1 (last (.split s2))))
      (and s1 (= s2 (last (.split s1))))))

(defn similar [s1 s2 [threshold 0.8]] ; -> bool
  "Two strings are heuristically similar, based on Jaro-Winkler algorithm and/or being the last word."
  (let [cs1 (sstrip (str s1))
        cs2 (sstrip (str s2))
        score (let [jw-score (jaro.jaro-winkler-metric cs1 cs2)]
                (if (last-word? cs1 cs2)
                    (+ jw-score 0.4)
                    jw-score))]
    (> score threshold)))

(defn best-of [l s]
  "Pick from l the most similar to s, based on Jaro-Winkler algorithm."
  (let [cs (sstrip s)
        scores (sorted (lfor x l [(jaro.jaro-winkler-metric cs (sstrip x)) x]))]
    (when scores
      (last (last scores)))))

(defn fuzzy-in [s l #** kwargs]
  "Fuzzy match to any of the items. Return best match or None."
  (when (any (map (partial similar s #** kwargs) l))
        (best-of l s)))

(defn fuzzy-substr [s long-s #** kwargs]
  "Fuzzy match if s is a substring of long-s. Return best match or None."
  (fuzzy-in s (.split long-s)))

(defn grep-attribute [s attribute]
  "Get an attribute from a string. Return as k-v tuple.
The attribute should be on its own line as:
`attribute: value`."
  (let [kvs (lfor l (.split s "\n")
                :if (similar attribute
                             (first (.partition l ":")))
                (.strip (last (.partition l ":"))
                        "\"' \n\t"))]
    (when kvs
      #(attribute (first kvs)))))

(defn grep-attributes [s attributes]
  "Get named attributes from a string. Return as dict."
  (dict (filter None (map (partial grep-attribute s) attributes))))
  
(defn format-msg [message] 
  "Format a chat or dialogue message as a string."
  (let [l (-> message
              (:role)
              (.capitalize)
              (+ ":"))
        content (-> message
                    (:content)
                    (.strip))]
    f"{l :<3} {(.strip (:content message))}"))

(defn format-msgs [messages]
  "Format a chat or dialogue as a long string."
  (.join "\n"
         (map format-msg messages)))
