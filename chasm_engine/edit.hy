"
Rudimentary editor of a json entry.
"

(require hyrule.argmove [-> ->>])

(import os)
(import subprocess)
(import json)
(import tempfile [NamedTemporaryFile])

(import chasm-engine.types [Coords])
(import chasm-engine.state [get-character update-character
                            get-place update-place
                            get-item update-item
                            get-account update-account])


(defn edit [d]
  "Edit a dict as json. Use system editor if defined, else rlinput."
  (let [editor (os.getenv "EDITOR" "vi")
        editor "/usr/bin/nvim"
        data (json.dumps d :indent 4)]
    (try
      (with [tf (NamedTemporaryFile :suffix ".json" :mode "w+" :delete False)]
        (tf.write data)
        (tf.flush)
        (subprocess.run [editor tf.name] :check True)
        (tf.seek 0)
        (json.loads (tf.read))))))

(defn edit-account [n]
  (let [a (get-account n)]
    (when a
      (update-account n #** (edit a)))))

(defn edit-character [n]
  (let [c (get-character n)]
    (when c
      (update-character c #** (edit (c._asdict))))))

(defn edit-item [n]
  (let [i (get-item n)]
    (when i
      (update-item i #** (edit (i._asdict))))))

(defn edit-place [x y]
  (let [c (Coords x y)
        p (get-place c)]
    (when c
      (update-place p #** (edit (p._asdict))))))
