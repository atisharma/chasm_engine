"
Functions that deal with characters.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import time [time])

(import chasm_engine [log])

(import chasm_engine.stdlib *)
(import chasm_engine.constants [alphabet default-character banned-names])
(import chasm_engine.types [Coords Character Item at?
                            mutable-character-attributes
                            initial-character-attributes])
(import chasm_engine [place memory])

(import chasm_engine.state [world path
                            get-place
                            get-accounts
                            get-character set-character update-character character-key get-characters])
(import chasm_engine.chat [respond yes-no
                           complete-json complete-lines
                           token-length truncate
                           user assistant system
                           msgs->dlg])


(defclass CharacterError [Exception])

(defn valid-key? [s]
  (re.match "^[a-zA-Z0-9][a-zA-Z0-9._-]*[a-zA-Z0-9]$" s))

(defn/a spawn [[name None] [coords (Coords 0 0)] [loaded {}] [retries 0]] ; -> Character
  "Spawn a character from card, db, or just generated."
  (try
    (let [; only allow to override some
          sanitised {"name" name
                     "appearance" (:appearance loaded None)
                     "gender" (:gender loaded None)
                     "backstory" (:backstory loaded None)
                     "voice" (:voice loaded None)
                     "traits" (:traits loaded None)
                     "likes" (:likes loaded None)
                     "dislikes" (:dislikes loaded None)
                     "occupation" (:occupation loaded None)
                     "motivation" (:motivation loaded None)}]
      (let [char (or (get-character name)
                     (await (gen-lines coords name))
                     default-character)
            filtered (dfor [k v] (.items sanitised) :if v k v)
            character (Character #** (| (._asdict default-character)
                                        (._asdict char)
                                        filtered))]
        (log.info f"{char.name}")
        (when loaded (log.info f"loaded: {sanitised}"))
        (if (and character.name
                 (valid-key? (character-key character.name))
                 (< retries 5))
            (do
              (log.info f"set character {name} -> {char.name}")
              ;(log.info (json.dumps (._asdict character))))
              (set-character character))
            (do
              ; else keep trying until it works
              (log.warn f"invalid spawn for character {character.name} at {coords}, retrying...")
              (await (spawn name coords loaded (inc retries)))))))
    (except [e [Exception]]
      (log.error f"spawn failed for {name} at {coords}.")
      (log.error e))))

(defn/a gen-lines [coords [name None]] ; -> Character or None
  "Make up some plausible character based on a name."
  (let [seed (choice alphabet)
        name-str (or name f"the character (invent one whose first name begins with '{seed}')")
        place (get-place coords)
        place-name (if place place.name "a typical place in this world")
        card f"name: '{name-str}'
motivation: 'drives their behaviour, 4-5 words
gender: 'their gender'
traits: 'shapes behaviour, MBTI, quirks/habits, 4-5 words'
skills: 'what they are particularly good at'
occupation: 'their usual job'
backstory: 'their backstory (10 words, memorable)'
likes: 'their desires, wants, cravings, guiding philosopher'
dislikes: 'their fears and aversions'
voice: 'their manner of speaking, 2-3 words'
appearance: 'their appearance, age, height, build, clothes, style etc (unique and memorable)'
objective: 'their current objective'"
        setting f"Story setting: {world}"
        instruction f"Below is a story setting and a template character card.
Complete the character card for {name-str} whom is found in the story at {place.name}.
Motivation must include a narrative archetype like Hero, Mentor, Villain, Informant, Guardian.
Make up a brief few words, with comma separated values, for each attribute. Be imaginative and very specific."
        details (await (complete-lines
                         :context setting
                         :template card
                         :instruction instruction
                         :attributes initial-character-attributes))
        name (word-chars (or name (:name details "")))
        objective (word-chars (:objective details ""))
        name-dict (if name {"name" name} {})]
    (log.info name)
    (Character #** (| (._asdict default-character)
                      details
                      name-dict
                      {"coords" coords
                       "objective" objective}))))

(defn/a gen-json [coords [name None]] ; -> Character or None
  "Make up some plausible character based on a name."
  (let [seed (choice alphabet)
        name-str (or name f"the character (whose first name begins with {seed})")
        name-dict (if name {"name" name} {})
        place (get-place coords)
        place-name (if place place.name "a typical place in this world")
        card f"{{
    \"name\": \"{name-str}\",
    \"motivation\": \"drives their behaviour, 4-5 words\",
    \"gender\": \"their gender\",
    \"traits\": \"shapes their behaviour, 4-5 words\",
    \"skills\": \"what they are particularly good at\",
    \"occupation\": \"their usual job\",
    \"backstory\": \"their backstory (10 words, memorable)\",
    \"likes\": \"their desires, wants and cravings\",
    \"dislikes\": \"their fears and aversions\",
    \"voice\": \"their manner of speaking, 2-3 words\",
    \"appearance\": \"{name-str}'s appearance, age, height, build, clothes, style etc (unique and memorable)\",
    \"objective\": \"their initial objective\"
}}"
        setting f"Story setting: {world}"
        instruction f"Below is a story setting and a character card.
Complete the character card for {name-str} whom is found in the story at {place.name}.
Motivation must include a narrative archetype like Hero, Mentor, Villain, Informant, Guardian.
Make up a brief few words, comma separated, for each attribute. Be imaginative and very specific."
        details (await (complete-json
                         :template card
                         :context setting
                         :instruction instruction))]
    (when details
      (log.info f"'{(:name details None)}'")
      (Character #** (| (._asdict default-character)
                        details
                        name-dict
                        {"coords" coords})))))

(defn describe [character [long False]]
  "A string that briefly describes the character."
  (if character
      (let [attributes (._asdict character)]
        ; pop off the things we don't want to inject
        (.pop attributes "coords")
        (.pop attributes "destination")
        (if long
            (json.dumps
              (dfor #(k v) (.items attributes)
                    :if v
                    k v)
              :indent 4)
            f"{character.name} - {character.appearance}"))
      ""))

(defn describe-at [coords [long False] [exclude None]]
  "A string describing any characters at a location."
  (let [all-at (get-at coords :exclude exclude)]
    (if all-at
        (.join "\n" ["The following characters (and nobody else) are here with you:"
                     #* (map (fn [c] (describe c :long long)) all-at)])
        "")))
  
(defn list-at-str [coords [exclude None]]
  "Give the names (as prose) of who is here."
  (let [character-names-here (lfor c (get-at coords :exclude exclude) c.name)
        n (len character-names-here)]
    (cond (= n 0) ""
          (= n 1) f"{(first character-names-here)} is here."
          :else f"{(.join ", " (butlast character-names-here))} and {(last character-names-here)} are here.")))

(defn get-at [coords [exclude None]]
  "List of characters at a location, excluding player.
This loops over all characters."
  (let [cs (get-characters)]
    (if cs
      (lfor character cs
            :if (and (at? coords character.coords)
                     (not (= character.name exclude)))
            character)
      []))) 

(defn move [character coords]
  "Just set a location."
  (update-character character :coords coords)
  coords)

(defn/a increment-score? [character messages]
  "Has the character done something worthwhile?"
  (let [setting f"Story setting: {world}"
        objective f"In the narrative, {character.name} has the following objective: {character.objective}"
        query f"Based only on events happening in the last two messages, has {character.name} done anything notable enough to increase their score?"
        msgs (truncate messages :spare-length 200)
        dialogue (await (msgs->dlg "narrator" character.name msgs))
        verdict (await (yes-no [(system query)
                                (user setting)]
                               :context (.join "\n\n" [objective (format-msgs dialogue)])
                               :query query))]
    (log.info f"{verdict}")
    verdict))

(defn/a develop-lines [character dialogue]
  "Develop a character's attributes based on the dialogue."
  (let [nearby-places (.join ", " (await (place.nearby character.coords :name True)))
        card f"name: {character.name}
appearance: {character.appearance}
health: {character.health}
emotions: {character.emotions}
destination: {character.destination}
objective: {character.objective}
new_memory: [classification] - any significant or poignant thing worth remembering from the dialogue"
        instruction f"You will be given a template character card for {character.name}, and the transcript of a dialogue involving them, for context.
Update any attribute that has changed describing the character, appropriate to the given context.
Appearance may change where a character changes clothes etc.
Destination should be just one of {nearby-places} or to stay at {(place.name character.coords)}.
Objective should align with the where the plot needs to go long-term, and the character's role (less than 8 words). It must give the character urgency and purpose. Either keep it the same or, if it's stale or superseded by a more important one, abandon it for a new mission.
Classify new memories into [significant], [minor] or [forgettable] (note square brackets).
Don't mention the original attribute if it is unchanged.
Use a brief few words, comma separated, for each attribute. Be concise and very specific."
        length (+ 200 (token-length [instruction world card card])) ; count card twice to allow the result
        dialogue-str (format-msgs (truncate dialogue :spare-length length))
        context f"Story setting: {world}

The dialogue is as follows:
{dialogue-str}"
        details (await (complete-lines
                         :context context
                         :template card
                         :instruction instruction
                         :attributes (append "new_memory" mutable-character-attributes)))
        objective (word-chars (.pop details "objective" ""))]
    (try
      (let [new-score (if (similar (or character.objective "")
                                   (:objective details "")
                                   :threshold 0.7)
                          character.score
                          (inc character.score))]
        (log.info f"{character.name}")
        (.pop details "name" None) ; leave the name alone
        (remember character (.pop details "new_memory" ""))
        (update-character character
                          :score new-score
                          :objective objective
                          #** details))
      (except [e [Exception]]
        ; generating to template sometimes fails 
        (log.error "Bad character" e)
        (log.error details)))))

(defn remember [character new-memory]
  "Commit memory to vector db."
  (log.info f"{character.name} {new-memory}")
  (let [mem-class (re.search r"\[(\w+)\]" new-memory)
        mem-point (re.search r"\][- ]*([\w ,.']+)" new-memory)]
    (when (and mem-class
               mem-point
               ; ignore failed memories
               (not (in "significant or poignant thing worth remembering from this dialogue" new-memory))
               (not (in "[forgettable]" new-memory))
               (not (in "[classification]" new-memory)))
      (memory.add (character-key character.name)
                  {"character" character.name
                   "coords" (str character.coords)
                   "place" (place.name character.coords)
                   "time" f"{(time):015.2f}"
                   "classification" (.lower (first (.groups mem-class)))}
                  (first (.groups mem-point))))))

(defn recall [character text [n 6] [class "significant"]]
  "Recall memories of a character. Pass `class=None` for all memories."
  (first
    (:documents (memory.query (character-key character.name)
                              :text text
                              :n n
                              :where (when class {"classification" class})))))

(defn/a get-new [messages player]
  "Are any new or existing characters mentioned in the messages?
They will appear at the player's location."
  ; messages will already have been truncated
  (let [setting (system f"Story setting: {world}")
        place-name (place.name player.coords)
        prelude (system f"Give a list of names of individuals (if any), one per line, that are obviously referred to in the text as being physically present at the current location ({place-name}) and time. Do not invent new characters. Exclude places and objects, only people's proper names count, no pronouns. Give the names as they appear in the text. Setting and narrative appear below.")
        instruction (user "Now, give the list of characters.")
        disallowed (+ [player.name place-name] banned-names)
        char-list (await (respond (->> messages
                                       (prepend setting)
                                       (prepend prelude)
                                       (append instruction)
                                       (truncate :spare-length 200)
                                       (append (assistant "The list of character names is:")))
                                  :max-tokens 50))
        filtered-char-list (->> char-list
                                 (debullet)
                                 (.split :sep "\n")
                                 (map capwords)
                                 (sieve)
                                 (filter (fn [x] (not (fuzzy-in x disallowed))))
                                 (filter (fn [x] (< (len (.split x)) 3))) ; exclude long rambling non-names
                                 (filter valid-key?)
                                 (list))]
    (log.info f"{filtered-char-list}")
    (cut filtered-char-list 3)))
