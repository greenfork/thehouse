(import spork/misc)
(import ./log)
(import ./text)
(use jaylib)
(use ./globals)
(use ./vector)

(def text-in-level-screen-offset
  [(math/round (* screen-width (/ 1 6)))
   (math/round (* screen-height (/ 3 4)))])

(defn flicker [obj color]
  (def cur-draw (obj :draw))
  (def cur-color (obj :color))
  (def cur-collision-cb (obj :collision-cb))
  (when (not= cur-color color)
    (put obj :color color)
    (put obj :draw nil)
    (ev/spawn
      (ev/sleep 0.2)
      (put obj :color cur-color)
      (put obj :draw cur-draw))))
(defn change-color [obj color]
  (set (obj :color) color))

(defn apply-offset [x map-offset]
  (v+= (x :pos) map-offset)
  x)
(defn unitize-obj [x size]
  (v*= (x :pos) [size size])
  x)

# Objects

(def hero-side (u 5))
(def Hero
  @{:id nil
    :type :hero
    :pos @[0 0]
    :dims @[hero-side hero-side]
    :bb (fn [self] @[(self :pos) (v+ (self :dims) (self :pos))])
    :move (fn [self v] (v+= (self :pos) v))
    :draw (fn [self] (draw-rectangle ;(map math/round (self :pos)) ;(self :dims) :maroon))})
(defn <hero> [& pairs] (misc/make Hero ;pairs))

(def Block
  @{:id nil
    :type :block
    :pos @[0 0]
    :dims @[block-side block-side]
    :color :yellow
    :bb (fn [self] @[(self :pos) (v+ (self :dims) (self :pos))])
    :collision-cb (fn [self] (flicker self :magenta) true)
    :draw (fn [self] (draw-rectangle ;(self :pos) ;(self :dims) (self :color)))})
(defn <block> [& pairs] (misc/make Block ;pairs))
(defn <door> [& pairs] (misc/make Block :color :brown ;pairs))
(defn <exit-door> [& pairs]
  (misc/make Block :color :brown :type :exit-door ;pairs))

(def SpecialBlock (misc/make Block :active false))
(defn <one> [& pairs] (misc/make SpecialBlock :type :one ;pairs))
(defn <two> [& pairs] (misc/make SpecialBlock :type :two ;pairs))
(defn <three> [& pairs] (misc/make SpecialBlock :type :three ;pairs))
(defn <four> [& pairs] (misc/make SpecialBlock :type :four ;pairs))
(defn <five> [& pairs] (misc/make SpecialBlock :type :five ;pairs))
(defn <six> [& pairs] (misc/make SpecialBlock :type :six ;pairs))
(defn <seven> [& pairs] (misc/make SpecialBlock :type :seven ;pairs))
(defn <eight> [& pairs] (misc/make SpecialBlock :type :eight ;pairs))
(defn <nine> [& pairs] (misc/make SpecialBlock :type :nine ;pairs))

# Levels

(var- level-object-counter 0)
(defn- lin-col [patt] ~(* (line) (column) ,patt))
(defn- level-object
  ``Create an object from map assuming that the object has a `:pos` field.``
  [letter ctor]
  ~(cmt ,(lin-col letter) ,(fn [l c]
                             (ctor :pos @[(dec c) (dec l)]
                                   :id (++ level-object-counter)))))
(def level-grammar
  ~{:space "."
    :block ,(level-object "B" <block>)
    :hero ,(level-object "@" <hero>)
    :door ,(level-object "D" <door>)
    :exit-door ,(level-object "d" <exit-door>)
    :one ,(level-object "1" <one>)
    :two ,(level-object "2" <two>)
    :three ,(level-object "3" <three>)
    :four ,(level-object "4" <four>)
    :five ,(level-object "5" <five>)
    :six ,(level-object "6" <six>)
    :seven ,(level-object "7" <seven>)
    :eight ,(level-object "8" <eight>)
    :nine ,(level-object "9" <nine>)
    :cell (+ :block :space :hero :door :exit-door
             :one :two :three :four :five :six :seven :eight :nine)
    :newline (+ "\n" "\r\n")
    :row (* (some :cell) (? :newline))
    :main (some :row)})

(defn- newline? [x] (or (= x (chr "\n")) (= x (chr "\r"))))
(def Level
  @{:id nil
    :name nil
    :phase :init
    :state nil
    :ascii nil
    :hero nil
    :blocks nil
    :specials nil
    :width (fn [self] (find-index newline? (self :ascii)))
    :height (fn [self] (inc (count newline? (self :ascii))))
    :screen-offset (fn [self sw sh]
                     [(math/round (- (/ sw 2) (* (/ (:width self) 2) block-side)))
                      (math/round (- (/ sh 2) (* (/ (:height self) 2) block-side) (/ sh 8)))])})
(defn- type? [& types] (fn [obj] (has-value? types (obj :type))))
(defn <level> [id name ascii]
  (def level (misc/make Level :name name :ascii ascii))
  (def objects
    (->>
      ascii
      (peg/match level-grammar)
      (map |(unitize-obj $ block-side))
      (map |(apply-offset $ (:screen-offset level screen-width screen-height)))))
  (put level :hero (find (type? :hero) objects))
  (put level :blocks (filter (type? :block :exit-door) objects))
  (put level :specials (filter (type? :one :two :three :four :five
                                      :six :seven :eight :nine) objects))
  level)
(defn curstate [level]
  (in (level :state) (level :phase)))
(defn change-phase [level phase]
  (when (not (= phase (level :phase)))
    (log/info* :level-change-phase true :from (level :phase) :to phase)
    (put level :phase phase)))

# Callback helpers

(defn- open-exit-doors-cb [col]
  (fn [self]
    (each ed (filter (type? :exit-door) col)
      (array/remove col (find-index |(= ($ :id) (ed :id)) col)))))
(defn destroy-collision-cb [col]
  (fn [self]
    (each c col
      (put c :collision-cb false))))

# Logic helpers

(defn- text-logic [text curphase &opt on-finishing]
  @{:run (fn [self]
           (text/draw (text/layout (in (self :text) (self :idx)) text-width)
                      text-in-level-screen-offset))
    :advance (fn [self]
               (if (< (inc (self :idx)) (length (self :text)))
                 (do
                   (+= (self :idx) 1)
                   curphase)
                 (do
                   (when on-finishing (on-finishing self))
                   :default)))
    :text text
    :idx 0})
(defn- remove-from-level [level id]
  (when-let [idx (find-index |(= id ($ :id)) (level :blocks))]
    (array/remove (level :blocks) idx))
  (when-let [idx (find-index |(= id ($ :id)) (level :specials))]
    (array/remove (level :specials) idx)))

###########
# Hallway #
###########

(def hallway-ascii ``
.......DD.........
.BBBBBB..BBBBBBBB.
.B..............B.
D@..............B.
D................d
.B...............d
.B..............B.
.B..............B.
.BBBBBBB..BBBBBBB.
........DD......
``)

(def hallway (<level> :hallway "Hallway" hallway-ascii))
(each exit-door (filter (type? :exit-door) (hallway :blocks))
  (set (exit-door :collision-cb) (open-exit-doors-cb (hallway :blocks))))
(put-in hallway [:state :init] (text-logic (text/hallway-text "START") :init))

############
# Corridor #
############

(def corridor-ascii ``
BBBBBBBBBBBBBBBBBBBBB
D@...1........2.....d
D....1........2.....d
BBBBBBBBBBBBBBBBBBBBB
``)

(def corridor (<level> :corridor "Corridor" corridor-ascii))
(each exit-door (filter (type? :exit-door) (corridor :blocks))
  (set (exit-door :collision-cb) (open-exit-doors-cb (corridor :blocks))))
(put-in corridor [:state :init] (text-logic (text/corridor-text "START") :init))
(array/concat (corridor :blocks)
              (filter (type? :one :two) (corridor :specials)))
(each one (filter (type? :one) (corridor :specials))
  (put one :collision-cb (fn [self] (put corridor :phase :corridor) false))
  (put one :draw false))
(put-in corridor [:state :corridor]
        (text-logic (text/corridor-text "CORRIDOR")
                    :corridor
                    (destroy-collision-cb (filter (type? :one) (corridor :specials)))))
(each two (filter (type? :two) (corridor :specials))
  (put two :collision-cb (fn [self] (put corridor :phase :tunnel) false))
  (put two :draw false))
(put-in corridor [:state :tunnel]
        (text-logic (text/corridor-text "TUNNEL")
                    :tunnel
                    (destroy-collision-cb (filter (type? :two) (corridor :specials)))))

###################
# Touch the Stone #
###################

(def touch-the-stone-ascii ``
.BBBB1BBBBBBBBBBBBBBB.
.B..................B.
.B..................2.
.B..................B.
D@...................d
D....................d
.B..................B.
.B..................B.
.B..................B.
.BBBBBBBBBBBBBB3BBBBB.
``)

(def touch-the-stone (<level> :touch-the-stone "Touch the Stone" touch-the-stone-ascii))
(defn- reset-collision-cb [obj] (set (obj :collision-cb) (fn [self] true)))
(defn- set-change-color-cb [type color blocks specials]
  (def obj (find (type? type) specials))
  (set (obj :collision-cb)
       (fn [self]
         (log/debug* type "active")
         (change-color self color)
         (set (self :active) true)
         (reset-collision-cb self)
         (when (all |($ :active) (filter (type? :one :two :three) specials))
           ((open-exit-doors-cb blocks) self)
           (change-phase touch-the-stone :unlocked))
         true)))
(array/concat (touch-the-stone :blocks)
              (filter (type? :one :two :three) (touch-the-stone :specials)))
(set-change-color-cb :one :red (touch-the-stone :blocks) (touch-the-stone :specials))
(set-change-color-cb :two :green (touch-the-stone :blocks) (touch-the-stone :specials))
(set-change-color-cb :three :blue (touch-the-stone :blocks) (touch-the-stone :specials))
(put-in touch-the-stone [:state :init] (text-logic (text/touch-the-stone-text "START") :init))
(each ed (filter (type? :exit-door) (touch-the-stone :blocks))
  (put ed :collision-cb (fn [self] (change-phase touch-the-stone :the-door) true)))
(put-in touch-the-stone [:state :the-door]
        (text-logic (text/touch-the-stone-text "THE-DOOR")
                    :the-door
                    (fn [self] (each ed (filter (type? :exit-door) (touch-the-stone :blocks))
                                 (put ed :collision-cb nil)))))
(put-in touch-the-stone [:state :unlocked] (text-logic (text/touch-the-stone-text "UNLOCKED") :unlocked))

######################
# Dance on the Floor #
######################

(def dance-on-the-floor-ascii ``
.BBBBBBBBBBBBBBBBBBBB.
.B..................B.
.B....BBB1....BBB...B.
.B....B.B.....B.B...B.
D@....BBB.....BBB....d
D....................d
.B.........BB.......B.
.B.....2...BB.....3.B.
.B..................B.
.BBBBBBBBBBBBBBBBBBBB.
``)

(defn make-dance-on-the-floor []
  (def level (<level> :dance-on-the-floor "Dance on the Floor" dance-on-the-floor-ascii))
  (put-in level [:state :init] (text-logic (text/dance-on-the-floor-text "START") :init))
  (put-in level [:state :the-door] (text-logic (text/dance-on-the-floor-text "THE-DOOR") :the-door))
  (put-in level [:state :pressed] (text-logic (text/dance-on-the-floor-text "PRESSED") :pressed))
  (put-in level [:state :flickered] (text-logic (text/dance-on-the-floor-text "FLICKERED") :flickered))
  (put-in level [:state :lost] (text-logic (text/dance-on-the-floor-text "LOST") :lost))
  (put-in level [:state :dont-touch-walls] (text-logic (text/dance-on-the-floor-text "DONT-TOUCH-WALLS") :dont-touch-walls))
  (put-in level [:state :unlocked] (text-logic (text/dance-on-the-floor-text "UNLOCKED") :unlocked))
  (each ed (filter (type? :exit-door) (level :blocks))
    (put ed :collision-cb
         (fn [self]
           (each b (filter (type? :block) (level :blocks))
             (put b :collision-cb
                  (fn [self]
                    (change-phase level :dont-touch-walls)
                    (each bl (filter (type? :block) (level :blocks))
                      (put bl :collision-cb nil))
                    true)))
           (change-phase level :the-door)
           true)))
  (def one (find (type? :one) (level :specials)))
  (def two (find (type? :two) (level :specials)))
  (def three (find (type? :three) (level :specials)))
  (each special [one two three]
    (put special :draw false)
    (array/concat (level :blocks) special))
  (var pressed-event-fired false)
  (var flickered-event-fired false)
  (var lost-event-fired false)
  (def nothing-cb (fn [self] false))
  (def one-cb
    (fn [self]
      (log/debug* :one "active")
      (put self :active true)
      (put self :color :red)
      (put self :draw nil)
      (when (not pressed-event-fired)
        (set pressed-event-fired true)
        (change-phase level :pressed))
      (put self :collision-cb nothing-cb)
      false))
  (def two-cb
    (fn [self]
      (if (one :active)
        (do
          (log/debug* :two "active")
          (put self :active true)
          (put self :color :green)
          (put self :draw nil)
          (put self :collision-cb nothing-cb))
        (do
          (log/debug* :two "flickered")
          (when (not flickered-event-fired)
            (set flickered-event-fired true)
            (change-phase level :flickered))
          (flicker self :green)))
      false))
  (def three-cb
    (fn [self]
      (if (and (one :active) (two :active))
        (do
          (log/debug* :three "open-the-door")
          ((open-exit-doors-cb (level :blocks)) self)
          (change-phase level :unlocked)
          (put self :color :blue)
          (put self :draw nil)
          (put self :collision-cb nothing-cb))
        (do
          (log/debug* :three "flickered")
          (flicker self :blue)
          (if (and (not lost-event-fired)
                   (or (one :active) (two :active)))
            (do
              (set lost-event-fired true)
              (change-phase level :lost))
            (when (not flickered-event-fired)
              (set flickered-event-fired true)
              (change-phase level :flickered)))
          (each hatch [one two]
            (put hatch :active false)
            (put hatch :draw (fn [self] false)))
          (put one :collision-cb one-cb)
          (put two :collision-cb two-cb)))
      false))
  (put one :collision-cb one-cb)
  (put two :collision-cb two-cb)
  (put three :collision-cb three-cb)
  level)

############
# Clean me #
############

(def clean-me-ascii ``
.BBBBBBBBB.BBBBBBBBBB.
.B.1.....B.B2.......B.
.B.......B.B........B.
.BBB.....BBBBB......B.
D@...................d
D................1...d
.BBB.....BBBBB......B.
.B......1B.B........B.
.B.......B.B........B.
.BBBBBBBBB.BBBBBBBBBB.
``)

(defn make-clean-me []
  (def level (<level> :clean-me "Clean me" clean-me-ascii))
  (put-in level [:state :init] (text-logic (text/clean-me-text "START") :init))
  (put-in level [:state :the-door] (text-logic (text/clean-me-text "THE-DOOR") :the-door))
  (put-in level [:state :trash] (text-logic (text/clean-me-text "TRASH") :trash))
  (put-in level [:state :two-trash] (text-logic (text/clean-me-text "TWO-TRASH") :two-trash))
  (put-in level [:state :bin] (text-logic (text/clean-me-text "BIN") :bin))
  (put-in level [:state :take-out] (text-logic (text/clean-me-text "TAKE-OUT") :take-out))
  (put-in level [:state :take-out-2] (text-logic (text/clean-me-text "TAKE-OUT-2") :take-out-2))
  (put-in level [:state :done] (text-logic (text/clean-me-text "DONE") :done))
  (each ed (filter (type? :exit-door) (level :blocks))
    (put ed :collision-cb (fn [self] (change-phase level :the-door) true)))
  (put-in level [:state :has-trash] false)
  (def trashes (filter (type? :one) (level :specials)))
  (def bin (find (type? :two) (level :specials)))
  (array/concat (level :blocks) trashes bin)
  (var trash-event-fired false)
  (each trash trashes
    (put trash :color :gray)
    (put trash :collision-cb
         (fn [self]
           (if (get-in level [:state :has-trash])
             (do
               (change-phase level :two-trash)
               true)
             (do
               (when (not trash-event-fired)
                 (set trash-event-fired true)
                 (change-phase level :trash))
               (log/debug* :trash-taken true)
               (put-in level [:state :has-trash] true)
               (remove-from-level level (self :id))
               false)))))
  (put bin :color :green)
  (var bin-event-fired false)
  (var take-out-idx 0)
  (put bin :collision-cb
       (fn [self]
         (if (get-in level [:state :has-trash])
           (do
             (case take-out-idx
               0 (do (+= take-out-idx 1) (change-phase level :take-out))
               1 (do (+= take-out-idx 1) (change-phase level :take-out-2))
               2 (do ((open-exit-doors-cb (level :blocks)) self) (change-phase level :done)))
             (log/debug* :trash-thrown true)
             (put-in level [:state :has-trash] false))
           (do
             (when (not bin-event-fired)
               (set bin-event-fired true)
               (change-phase level :bin))))
         true))
  level)

#########
# Final #
#########

(def final-ascii ``
........BBBBBBBBBBBBB.
........B...........B.
........B...........B.
.BBBBBBBB........222B.
d@......1........29.B.
d.......1........29.B.
.BBBBBBBB........222B.
........B...........B.
........B...........B.
........BBBBBBBBBBBBB.
``)

(defn make-final [exit-game]
  (def level (<level> :final "Final" final-ascii))
  (put-in level [:state :init] (text-logic (text/final-text "START") :init))
  (put-in level [:state :try-exit] (text-logic (text/final-text "TRY-EXIT") :try-exit))
  (put-in level [:state :exit] (text-logic (text/final-text "EXIT") :exit (fn [_] (exit-game))))
  (put-in level [:state :closer]
          (text-logic (text/final-text "CLOSER") :closer
                      (destroy-collision-cb (filter (type? :one) (level :specials)))))
  (put-in level [:state :stone-table] (text-logic (text/final-text "STONE-TABLE")
                                                  :stone-table (fn [_] (exit-game))))
  (array/concat (level :blocks) (filter (type? :one :two :nine) (level :specials)))
  (each one (filter (type? :one) (level :specials))
    (put one :collision-cb (fn [self] (put level :phase :closer) false))
    (put one :draw false))
  (each two (filter (type? :two) (level :specials))
    (put two :collision-cb (fn [self] (put level :phase :stone-table) false))
    (put two :draw false))
  (each nine (filter (type? :nine) (level :specials))
    (put nine :color :gold))
  (each ed (filter (type? :exit-door) (level :blocks))
    (put ed :collision-cb (fn [self]
                            (change-phase level :try-exit)
                            (put self :collision-cb (fn [self]
                                                      (change-phase level :exit)
                                                      true))
                            true)))
  level)
