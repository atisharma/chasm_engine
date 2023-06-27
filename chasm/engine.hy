"
The game engine. Handles interaction between Place, Item, Character and Dialogue.
"

(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import chasm.stdlib *)
(import chasm [place item character])
(import chasm.state [world world-name news
                     username
                     update-character])
(import chasm.chat [respond
                    append prepend
                    user assistant system])
(import chasm.interface [spinner info error])


;;; TODO: maybe abstract out a standard narrator prompt template.

(defn move [coords dirn player [messages None]] ; -> msg-pair or None
  "Extend the map. Adjust the coordinates. Describe."
  ; dirn may be a compass direction like "n" or a place name like "Small House"
  (let [new-coords (place.go dirn coords)]
    (when new-coords
      (with [s (spinner "Travelling...")]
        (place.new new-coords)
        (character.move player new-coords)
        {"coords" new-coords
         "msg" (assistant (place.describe new-coords
                                          :messages messages
                                          :length "very short"))}))))

; TODO: enable looking at characters and items.
(defn look [coords [messages None] [length "short"]]
  "Describe the surroundings, a character, or an item."
  (with [s (spinner "Writing...")]
    (assistant (place.describe coords :messages messages :length length))))

(defn assess [messages coords line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.describe-at coords)
        characters-here (character.describe-at coords)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user (whose name is {username}), who is playing, interjects with questions, instructions or commands. These are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}
The user {username} cannot and will not go anywhere else, and does not want to go anywhere unless the narrator has been instructed.

{items-here}
{characters-here}

Ignore off-topic questions. Refuse instructions that are implausible or inconsistent with the lore. Do not give instructions, just be descriptive."
        instruction "Assess how plausible or consistent the instruction below is for the player to do in the context of the story. Give a score from 1 (least) to 10 (most) then explain your reasons."]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (append (user instruction))
           (append (user line))
           (respond)
           (trim-prose)
           (info)))))

(defn hint [messages coords line]
  (let [[cmd _ obj] (.partition line " ")
        items-here (item.describe-at coords)
        characters-here (character.describe-at coords :long True)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user, whose name is {username} and who is playing, interjects with questions, instructions or commands.
The assistant responds in the narrator's voice.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}

{items-here}
{characters-here}"
        instruction (+ "Give a single, one-sentence hint to progress the plot. "
                       (if line
                           f"The hint must relate to the following question: {line}"
                           "The hint should be a riddle."))]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (append (user instruction))
           (respond)
           (trim-prose)
           (+ "**Hint**: ")
           (info)))))

(defn what-if [messages coords line]
  (let [[cmd _ instruction] (.partition line " ")
        items-here (item.describe-at coords)
        characters-here (character.describe-at coords :long True)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The user, whose name is {username} and who is playing, interjects with questions, instructions or commands.
The assistant responds in the narrator's voice.

Story setting: {world}"
        local-guidance f"{(news)}
The user is at {(place.name coords)}.
These places are accessible:
{(place.nearby-str coords)}
{(place.rooms coords)}

{items-here}
{characters-here}"]
    (with [s (spinner "Postulating...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (append (user instruction))
           (respond)
           (trim-prose)
           (info)))))

(defn narrate [messages player]
  "Narrate the story, in the fictional universe."
  (let [items-here (item.describe-at player.coords)
        character-names-here (lfor c (character.get-at player.coords) c.name)
        detailed-chars (any (map (fn [s] (in (.lower s) (.lower (:content (last messages)))))
                                 character-names-here))
        characters-here (character.describe-at player.coords :long detailed-chars)
        story-guidance f"The assistant is a narrator in a gripping and enjoyable adventure game.
The player interjects with questions, instructions or commands. These commands are always meant in the context of the story.
The assistant responds as the narrator, and must never break the 'fourth wall'.
The player is sometimes called 'user' or '{player.name}' - these refer to the same person whom is referred to in the second person by the assistant. If naming the player, only ever refer to them as {player.name}.

Story setting: {world}"
        local-guidance f"{(news)}
The player is still at the {(place.name player.coords)}. Do not let them go anywhere else - it would break the story.

These places are accessible:
{(place.nearby-str player.coords)}
{(place.rooms player.coords)}

{items-here}
{characters-here}

Assess how plausible, on-topic or consistent the instruction below is in context of the storyline. If the instruction is highly inconsistent (for example 'turn into a banana' when that's impossible), just say 'You can't do that' or some humorous variation. Make every effort to keep the story consistent.
Otherwise, follow the instruction.
The narrator does not give instructions, just be descriptive."]
    (with [s (spinner "Writing...")]
      (->> messages
           (prepend (system story-guidance))
           (append (system local-guidance))
           (respond)
           (trim-prose)
           (assistant)))))
