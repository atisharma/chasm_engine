"
Develop the plot / world events

- extract plot points from the messages
- summarise into
  * new important world facts
  * plot points
  * temporal story development
- global quests / mysteries (not character ones)
"

(import time [time])
(import datetime [datetime timezone])
        
(import chasm_engine.stdlib *)
(import chasm_engine.types *)

(import chasm_engine [log])

(import chasm_engine [place character memory])
(import chasm_engine.state [world])
(import chasm_engine.chat [truncate respond
                           system user assistant
                           msgs->dlg])


;;; -----------------------------------------------------------------------------

(defn now []
  "Just the time."
  (.strftime (datetime.now timezone.utc) "%H:%M, %a %d %h"))

(defn news []
 "Up-to-date info about the universe."
 (.join "\n"
        [f"It is {(.strftime (datetime.now timezone.utc) "%H:%M, %a %d %h")}."
         #* (recent :where {"classification" "major"})]))

(defn recent [[n 5] [where None]]
  "Return `n` recent memories filtered by `where` (e.g. `{\"classification\" \"major\"}"
  (:documents (memory.recent "narrator" :n n :where where)))

(defn recall-points [text [n 6] [class "major"]]
  "Recall the important plot points relating to `text` (the player, story thread, characters present, etc.)."
  (first
    (:documents (memory.query "narrator"
                              :text text
                              :n n
                              :where (when class {"classification" class})))))

(defn/a extract-point [messages player]
  "Scan the recent conversation for plot points and insert them into the record."
  (let [msgs (truncate (cut messages -6 None) :spare-length 200)
        narrative (format-msgs (msgs->dlg "narrative" player.name msgs))
        instruction "In the following narrative, classify events from the last two messages for significance to the story's narrative arc, as [major], [minor], [subplot] or [irrelevant]. Major events are relevant to more than one character or to the fictional world as a whole. Most will be minor or subplot. Use the square bracket format. Give a single concise bullet point (max 15 words) describing the plot point. Stay faithful to the story setting. If events relate to people, use their names. Give the classification, then the point. Don't justify your answer."
        setting f"Story setting: {world}"
        response (await (respond [(system instruction)
                                  (user (.join "\n\n" [setting narrative "Give the plot point."]))
                                  (assistant "The classification and plot point is:")]
                                 :max_tokens 100))
        sanitised (.replace response "\n" " ")
        classification (re.search r"\[(\w+)\]" sanitised)
        point (re.search r"\][- ]*([\w ,.'-]+)" sanitised)]
    (when (and point classification)
      (log.info f"{(first (classification.groups))}")
      (log.info f"{(first (point.groups))}")
      (let [chars-here (lfor c (append player (character.get-at player.coords)) c.name)
            meta {"time" f"{(time):015.2f}"
                  "place" (place.name player.coords)
                  "coords" (str player.coords)
                  "characters" (.join " " chars-here)
                  "classification" (.lower (first (classification.groups)))}
            pt (first (point.groups))]
        (when (in classification ["major" "minor" "subplot"])
          (new-event f"{chars-here} {pt}"))
        (memory.add "narrator"
                    :metadata meta
                    :text pt)))))
