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
  (def cur-color (obj :color))
  (when (not= cur-color color)
    (set (obj :color) color)
    (ev/spawn
      (ev/sleep 0.2)
      (set (obj :color) cur-color))))
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
    :row (* (some :cell) (? "\n"))
    :main (some :row)})

(defn- newline? [x] (= x (chr "\n")))
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
           ((open-exit-doors-cb blocks) self))
         true)))
(array/concat (touch-the-stone :blocks)
              (filter (type? :one :two :three) (touch-the-stone :specials)))
(set-change-color-cb :one :red (touch-the-stone :blocks) (touch-the-stone :specials))
(set-change-color-cb :two :green (touch-the-stone :blocks) (touch-the-stone :specials))
(set-change-color-cb :three :blue (touch-the-stone :blocks) (touch-the-stone :specials))
(put-in touch-the-stone [:state :init] (text-logic (text/touch-the-stone-text "START") :init))
