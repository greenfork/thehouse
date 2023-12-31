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

(defn move-hero-a-bit
  "Move a hearo so that its positioning looks more natural."
  [x]
  (when (= (x :type) :hero)
    (v+= (x :pos) [0.2 0.5]))
  x)
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
(defn- newline-count? [x] (= x (chr "\n")))
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
    :height (fn [self] (inc (count newline-count? (self :ascii))))
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
      (map move-hero-a-bit)
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

(def hallway-ascii (slurp "assets/levels/hallway.txt"))

(defn make-hallway []
  (def level (<level> :hallway "Hallway" hallway-ascii))
  (each exit-door (filter (type? :exit-door) (level :blocks))
    (set (exit-door :collision-cb) (open-exit-doors-cb (level :blocks))))
  (put-in level [:state :init] (text-logic ((text/get "hallway") "START") :init)))


############
# Corridor #
############

(def corridor-ascii (slurp "assets/levels/corridor.txt"))

(defn make-corridor []
  (def level (<level> :corridor "Corridor" corridor-ascii))
  (each exit-door (filter (type? :exit-door) (level :blocks))
    (set (exit-door :collision-cb) (open-exit-doors-cb (level :blocks))))
  (put-in level [:state :init] (text-logic ((text/get "corridor") "START") :init))
  (array/concat (level :blocks)
                (filter (type? :one :two) (level :specials)))
  (each one (filter (type? :one) (level :specials))
    (put one :collision-cb (fn [self] (put level :phase :corridor) false))
    (put one :draw false))
  (put-in level [:state :corridor]
          (text-logic ((text/get "corridor") "CORRIDOR")
                      :corridor
                      (destroy-collision-cb (filter (type? :one) (level :specials)))))
  (each two (filter (type? :two) (level :specials))
    (put two :collision-cb (fn [self] (put level :phase :tunnel) false))
    (put two :draw false))
  (put-in level [:state :tunnel]
          (text-logic ((text/get "corridor") "TUNNEL")
                      :tunnel
                      (destroy-collision-cb (filter (type? :two) (level :specials))))))

###################
# Touch the Stone #
###################

(def touch-the-stone-ascii (slurp "assets/levels/touch_the_stone.txt"))

(defn make-touch-the-stone []
  (def level (<level> :touch-the-stone "Touch the Stone" touch-the-stone-ascii))
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
             (change-phase level :unlocked))
           true)))
  (array/concat (level :blocks)
                (filter (type? :one :two :three) (level :specials)))
  (set-change-color-cb :one :pink (level :blocks) (level :specials))
  (set-change-color-cb :two :green (level :blocks) (level :specials))
  (set-change-color-cb :three :blue (level :blocks) (level :specials))
  (put-in level [:state :init] (text-logic ((text/get "touch-the-stone") "START") :init))
  (each ed (filter (type? :exit-door) (level :blocks))
    (put ed :collision-cb (fn [self] (change-phase level :the-door) true)))
  (put-in level [:state :the-door]
          (text-logic ((text/get "touch-the-stone") "THE-DOOR") :the-door))
  (put-in level [:state :unlocked] (text-logic ((text/get "touch-the-stone") "UNLOCKED") :unlocked)))


######################
# Dance on the Floor #
######################

(def dance-on-the-floor-ascii (slurp "assets/levels/dance_on_the_floor.txt"))

(defn make-dance-on-the-floor []
  (def level (<level> :dance-on-the-floor "Dance on the Floor" dance-on-the-floor-ascii))
  (put-in level [:state :init] (text-logic ((text/get "dance-on-the-floor") "START") :init))
  (put-in level [:state :the-door] (text-logic ((text/get "dance-on-the-floor") "THE-DOOR") :the-door))
  (put-in level [:state :pressed] (text-logic ((text/get "dance-on-the-floor") "PRESSED") :pressed))
  (put-in level [:state :flickered] (text-logic ((text/get "dance-on-the-floor") "FLICKERED") :flickered))
  (put-in level [:state :lost] (text-logic ((text/get "dance-on-the-floor") "LOST") :lost))
  (put-in level [:state :dont-touch-walls] (text-logic ((text/get "dance-on-the-floor") "DONT-TOUCH-WALLS") :dont-touch-walls))
  (put-in level [:state :unlocked] (text-logic ((text/get "dance-on-the-floor") "UNLOCKED") :unlocked))
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
      (put self :color :pink)
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

(def clean-me-ascii (slurp "assets/levels/clean_me.txt"))

(defn make-clean-me []
  (def level (<level> :clean-me "Clean me" clean-me-ascii))
  (put-in level [:state :init] (text-logic ((text/get "clean-me") "START") :init))
  (put-in level [:state :the-door] (text-logic ((text/get "clean-me") "THE-DOOR") :the-door))
  (put-in level [:state :trash] (text-logic ((text/get "clean-me") "TRASH") :trash))
  (put-in level [:state :two-trash] (text-logic ((text/get "clean-me") "TWO-TRASH") :two-trash))
  (put-in level [:state :bin] (text-logic ((text/get "clean-me") "BIN") :bin))
  (put-in level [:state :take-out] (text-logic ((text/get "clean-me") "TAKE-OUT") :take-out))
  (put-in level [:state :take-out-2] (text-logic ((text/get "clean-me") "TAKE-OUT-2") :take-out-2))
  (put-in level [:state :done] (text-logic ((text/get "clean-me") "DONE") :done))
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

(def final-ascii (slurp "assets/levels/final.txt"))

(defn make-final [exit-game]
  (def level (<level> :final "Final" final-ascii))
  (put-in level [:state :init] (text-logic ((text/get "final") "START") :init))
  (put-in level [:state :try-exit] (text-logic ((text/get "final") "TRY-EXIT") :try-exit))
  (put-in level [:state :exit] (text-logic ((text/get "final") "EXIT") :exit (fn [_] (exit-game))))
  (put-in level [:state :closer]
          (text-logic ((text/get "final") "CLOSER") :closer
                      (destroy-collision-cb (filter (type? :one) (level :specials)))))
  (put-in level [:state :stone-table] (text-logic ((text/get "final") "STONE-TABLE")
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
    (put ed :draw false)
    (put ed :collision-cb (fn [self]
                            (change-phase level :try-exit)
                            (put self :collision-cb (fn [self]
                                                      (change-phase level :exit)
                                                      true))
                            true)))
  level)
