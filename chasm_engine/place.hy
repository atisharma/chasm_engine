"
Functions that manage place.
"

(require hyrule [-> ->>])
(require hyrule [unless])

(import hyjinx [extract-json])

(import tenacity [retry stop-after-attempt wait-random-exponential])

(import chasm_engine [log])

(require chasm_engine.instructions [deftemplate def-fill-template])

(import chasm_engine.lib *)
(import chasm_engine.constants [compass-directions alphanumeric place-types place-attributes])
(import chasm_engine.state [world get-place set-place update-place])
(import chasm_engine.types [Coords Place])
(import chasm_engine.chat [msgs->dlg system user]) 


(defclass ChasmPlaceError [RuntimeError])

(deftemplate place)
(deftemplate context)
(def-fill-template place description description-system)
(def-fill-template place rooms rooms-system)
(def-fill-template place guess-room)
(def-fill-template place json new)
;(def-fill-template place lines new)
(def-fill-template place accessible)

;; Anything -> bool
;; -----------------------------------------------------------------------------

(defn nearby? [coords1 coords2 [distance 1]]
  "Is coord1 within a distance of coord2 (inclusive)?"
  (and (<= (abs (- (:x coords1) (:x coords2))) distance)
       (<= (abs (- (:y coords1) (:y coords2))) distance)))

(defn :async [(alru-cache :maxsize 1000)] accessible? [placename destination]
  "Is a destination accessible to the player?
  We cache this both for performance and persistence of place characteristics."
  (let [response (await (place-accessible
                          :world world
                          :placename placename
                          :destination destination))]
    (or (similar response "yes")
        (in "yes" (.lower response)))))

;; Place prompts -> text
;; -----------------------------------------------------------------------------

(defn :async chat-gen-description [nearby-str place player messages [length "very short"]]
  "Make up a short place description from its name."
  (-> (place-description
        messages
        :world world
        :place-name place.name
        :place place
        :player player.name
        :nearby nearby-str
        :length length)
    (await)
    (trim-prose)))

(defn :async gen-description [nearby-str coords [world-str world]]
  "Make up a single-paragraph place description from its name."
  (let [place (get-place coords)
        response (await (place-description
                          :world world
                          :place-name place.name
                          :place place
                          :player "you"
                          :nearby nearby-str
                          :length "one paragraph"))]
    (.join "\n\n"
           [f"**{place.name}**"
            (-> response
                (.replace "\"" "")
                trim-prose)])))

(defn :async gen-json [nearby-places]
  "Make up a place from its neighbours."
  (let [details (extract-json
                  (await (place-json
                           :context f"The story's setting is: {world}\nNearby places: {nearby-places}"
                           :seed (choice alphanumeric)
                           :place-type (choice place-types))))]
    (when (and details (:name details None))
      {"name" (capwords (re.sub r"^[Tt]he " "" (:name details)))
       "appearance" (:appearance details None)
       "atmosphere" (:atmosphere details None)
       "terrain" (:terrain details None)})))

#_(defn :async gen-lines [nearby-places]
    "Make up a place from its neighbours."
    (let [seed (choice alphanumeric)
          details (grep-attributes
                    (await (place-lines
                             :context context
                             :seed seed
                             :place-type (choice place-types)))
                    place-attributes)
          name (word-chars (:name details ""))]
      (when (and name
                 (< (len (.split (:name details ""))) 4))
        (| details {"name" name}))))

(defn :async gen-rooms [place-dict]
  "Make up some rooms for a place."
  (extract-json
    (await
      (place-rooms
        :world world
        :place place-dict))))

;; FIXME: it's a bit flaky -- might be better with newer models -- test
(defn :async guess-room [messages coords]
  "Guess the player's room."
  (let [dlg (await (msgs->dlg "Player" "Narrator" messages))  
        room-list (rooms coords :as-string False)
        rooms-str (.join ", " room-list)]
    (if rooms-str
      (best-of
        room-list
        (await (place-guess-room
                 :dialogue dlg
                 :rooms rooms-str
                 :player player.name)))
      "")))

;; Place functions
;; -----------------------------------------------------------------------------

(defn rose [dx dy]
  "The word for the compass direction.
  `dx` is eastings, `dy` is northings."
  (match #(dx dy)
         #(0 1)   "north"
         #(1 0)   "east"
         #(-1 0)  "west"
         #(0 -1)  "south"
         #(1 1)   "northeast"
         #(1 -1)  "southeast"
         #(-1 -1) "southwest"
         #(-1 1)  "northwest"))

(defn :async go [dirn coords [allow-inaccessible False] [threshold 0.8]] ; str, Coords -> Coords or None
  "Interpret a string as a change in location.
  Match the string to a compass direction or a nearby place name.
  Return new coords or None."
  (let [d (-> dirn (.lower) (.strip))
        x (:x coords)
        y (:y coords)
        n (inc (:y coords))
        e (inc (:x coords))
        s (dec (:y coords))
        w (dec (:x coords))
        places (await (nearby coords
                              :place True
                              :list-inaccessible allow-inaccessible))
        place-names (lfor p places p.name)
        place-coords (lfor p places p.coords)
        fuzzy-match-name (fuzzy-in d place-names :threshold threshold)] 
    ; remember, Coords use eastings then northings
    (let [new-coords (cond (in d ["n" "north"]) (Coords x n)
                           (in d ["ne" "northeast"]) (Coords e n)
                           (in d ["e" "east"]) (Coords e y)
                           (in d ["se" "southeast"]) (Coords e s)
                           (in d ["s" "south"]) (Coords x s)
                           (in d ["sw" "southwest"]) (Coords w s)
                           (in d ["w" "west"]) (Coords w y)
                           (in d ["nw" "northwest"]) (Coords w n)
                           fuzzy-match-name (get (dfor p places p.name p.coords) fuzzy-match-name))]
      (when (in new-coords place-coords) new-coords))))

(defn get-offset-place [coords dx dy]
  (get-place (Coords (+ (:x coords) dx) (+ (:y coords) dy))))

(defn :async nearby [_coords [name False] [place False] [coords False] [place-dirn False] [list-inaccessible False]]
  "A list of all existing [place names + directions] in adjacent cells, accessible or not."
  (let [cx (:x _coords)
        cy (:y _coords)
        accessible-places (unless list-inaccessible (await (accessible _coords :min-places 4)))]
    (lfor dx [-1 0 1]
          dy [-1 0 1]
          :setv nearby-place (get-offset-place _coords dx dy)
          :if (and nearby-place
                   (+ (abs dx) (abs dy)) ; not the same place
                   (or list-inaccessible (in nearby-place accessible-places)))
          (cond name nearby-place.name
                place nearby-place
                coords nearby-place.coords
                place-dirn [nearby-place f"{nearby-place.name}, to the {(rose dx dy)}"]
                :else f"{nearby-place.name}, to the {(rose dx dy)}"))))

(defn :async nearby-str [coords #** kwargs]
  "A table of all existing [place names, directions]
  in adjacent cells, accessible or not."
  (.join "\n" (await (nearby coords #** kwargs))))
  
(defn :async new [coords]
  "Add a description etc, item, character to a place."
  ; TODO: consider pre-generating a long list of names, to ensure uniqueness.
  (let [near-places (.join ", " (await (nearby coords :list-inaccessible True :name True)))
        details (await (gen-json near-places))]
    (if (and details (:name details None))
        (let [m (->> (:name details)
                     (re.search r"([\w ]+)"))
              name (when m (-> m
                               (.groups)
                               (first)
                               (sstrip)
                               (capwords)))
              place (Place :coords coords
                           :name name
                           :rooms (await (gen-rooms details))
                           :atmosphere (:atmosphere details None)
                           :appearance (:appearance details None)
                           :terrain (:terrain details None))]
          (log.info f"{place.name} @ {coords}")
          (set-place place))
      (log.error f"generation failed @ {coords}\n{near-places}\n-> {details}"))))

(defn :async accessible [coords * min-places]
  "A list of the accessible Places the player can move to.
  If none are naturally accessible, pick a nearby few at random.
  This function is not deterministic, because we ask the model to decide."
  (let [place (get-place coords)
        near-places (await (nearby coords :place True :list-inaccessible True))
        dests (lfor dest near-places
                    (when (and place (await (accessible? place.name dest.name)))
                          dest))]
    (or (list (sieve dests))
        (cut (sorted near-places
                     :key (fn [p] (hash-id p.name)))
             min-places))))

(defn :async accessible-coords [coords [min-places 4]]
  "A list of all existing Coords that are accessible from here."
  (lfor a (await (accessible coords :min-places min-places))
        a.coords))

(defn :async [(retry :stop (stop-after-attempt 4))] extend-map [coords]
  "Extend the map so neighbouring places exist."
  (let [cx (:x coords)
        cy (:y coords)
        places (lfor dx [-1 0 1]
                     dy [-1 0 1]
                     :setv _coords (Coords (+ cx dx) (+ cy dy))
                     (or (get-place _coords)
                         (await (new _coords))
                         (await (new _coords))))]
    (unless (get-place coords)
      (raise (ChasmPlaceError f"place/extend-map: unable to generate at {coords}")))))
    
(defn rooms [coords [as-string True]]
  (let [place (get-place coords)
        rooms-str (if place.rooms
                    (.join ", " place.rooms)
                    "")
        room-str (if place.rooms
                     f"{place.name} has the following rooms: {rooms-str}"
                     "")]
    (if as-string
        room-str
        place.rooms)))

(defn :async describe [player [messages None] [length "very short"]]
  "Return a description of the location."
  (let [coords player.coords
        place (get-place coords)
        near-str (.join "; " (await (nearby coords :list-inaccessible False :name True)))]
    (if messages
        (await (chat-gen-description near-str
                                     place
                                     player
                                     messages
                                     :length length))
        (await (gen-description near-str (str coords))))))

(defn name [coords]
  (. (get-place coords) name))
