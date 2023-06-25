(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm [log])

(import functools [partial lru-cache])
(import importlib)
(import os)
(import re)
(import json)
(import readline)
(import pathlib [Path])
(import hashlib [sha1 pbkdf2-hmac])
(import hmac [compare-digest])

(import jaro)

;; tomllib for python 3.11 onwards
;; when we move to 3.11, we can remove this
(try
  (import tomllib)
  (except [ModuleNotFoundError]
    (import tomli :as tomllib)))


;;; -----------------------------------------------------------------------------
;;; generic functions
;;; -----------------------------------------------------------------------------

(defn mreload [#* modules]
  "Reload a whole list of modules."
  (for [m modules]
    (try (importlib.reload m)
         (except [e [ImportError]] 
            (print e)))))

; TODO: rewrite as macros
(defn first [xs]
  (get xs 0))

(defn rest [xs]
  (cut xs 1 None))

(defn last [xs]
  (get xs -1))

(defn butlast [xs]
  (cut xs 0 -1))

(defn sieve [xs]
  (filter None xs))

;;; -----------------------------------------------------------------------------
;;; config functions
;;; -----------------------------------------------------------------------------

(setv config-file "config.toml")

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

(defn rlinput [prompt [prefill ""]]
  "Like python's input() but using readline."
  (readline.set_startup_hook (fn [] (readline.insert_text prefill)))
  (try
    (input prompt)
    (except [EOFError]
      "/quit")
    (finally
      (readline.set_startup_hook))))

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
;;; String functions
;;; -----------------------------------------------------------------------------

(defn sstrip [s]
  "Strip surrounding whitespace, quotes, '.',
force to lowercase, remove 'the' from start of line."
  (re.sub r"^the " ""
          (-> s
              (.strip "\n\t .\"'`")
              (.lower))))

(defn itemize [markdown-list] ; -> str
  "Get the main items from a markdown list."
  (->> markdown-list
       (re.sub r"^[\*\-\.(\[ \])\d]*" "" :flags re.M) ; remove bullet points
       (re.sub r"^([\w ']*\w).*$" r"\1" :flags re.M))) ; get main item
  
(defn trim-prose [s]
  "Remove any incomplete sentence."
  (let [paras (-> s
                  (.strip)
                  (->> (re.sub r"\n{3,}" r"\n" :flags re.M))
                  ;(.replace "\n\n\n" "\n\n")
                  (.split "\n\n")
                  (->> (filter (fn [x] (not (= x "."))))
                       (map (fn [x] (.strip x "\n\t\"'"))))
                  (sieve)
                  (list))]
    (.join "\n\n"
           (if (in (last (.strip (last paras))) ".?!")
               paras
               (+ (cut paras -1)
                  ; cut off last incomplete sentence
                  [(+ (.join "." (-> (last paras) (.split ".") (cut -1))) ".")])))))

(defn is-last-word [s1 s2]
  (or (= s1 (last (.split s2)))
      (= s2 (last (.split s1)))))

(defn similar [s1 s2 [threshold 0.8]] ; -> bool
  "Two strings are heuristically similar, based on Jaro-Winkler algorithm and/or being the last word."
  (let [cs1 (sstrip s1)
        cs2 (sstrip s2)
        score (let [jw-score (jaro.jaro-winkler-metric cs1 cs2)]
                (if (last-word? cs1 cs2)
                    (+ jw-score 0.4)
                    jw-score))]
    (> score threshold)))

(defn best-of [l s]
  "Pick from l the most similar to s, based on Jaro-Winkler algorithm."
  (let [cs (sstrip s)
        scores (sorted (lfor x l [(jaro.jaro-winkler-metric cs (sstrip x)) x]))]
    (last (last scores))))

(defn fuzzy-in [s l #** kwargs]
  "Fuzzy match to any of the items. Return best match or None."
  (when (any (map (partial similar s #** kwargs) l))
        (best-of l s)))
