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
        
(import chasm.stdlib *)
(import chasm.types *)

(import chasm [log])

(import chasm [place character])
(import chasm.state [now news world username
                     get-event
                     set-event])
(import chasm.chat [truncate respond
                    system user assistant
                    msgs->dlg])


(defn recall-points [messages player]
  "Recall the important plot points relating to the player, story thread, characters present.")


(defn extract-point [messages player]
  "Scan the recent conversation for plot points and insert them into the record."
  (let [msgs (truncate (cut messages -6 None) :spare-length 500)
        narrative (format-msgs (msgs->dlg "narrative" player.name msgs))
        instruction "In the following narrative, classify events from the last messages for significance to the story's narrative arc, as [crucial], [major], [minor], [subplot], [place] or [irrelevant]. Most will be minor. Use the square bracket format. Give a single bullet point (max 15 words) describing the plot point. Stay faithful to the story setting. Give the classification, then the point. Don't justify your answer."
        setting f"Story setting: {world}"
        response (respond [(system instruction)
                           (user setting)
                           (user narrative)
                           (user "Give the plot point.")
                           (assistant "The classification and plot point is:")]
                          :max_tokens 300)
        sanitised (.replace response "\n" " ")
        classification (re.search r"\[(\w+)\]" sanitised)
        point (re.search r"\][- ]*([\w ,.']+)" sanitised)]
    (when (and point classification)
      (log.info f"plot/extract-point: {(first (classification.groups))}")
      (log.info f"plot/extract-point: {(first (point.groups))}")
      (set-event (Event #** {"time" f"{(time):015.2f}"
                             "place" (place.name player.coords)
                             "coords" player.coords
                             "characters" (lfor c (append player (character.get-at player.coords)) c.name)
                             "point" (first (point.groups))
                             "classification" (.lower (first (classification.groups)))})))))
