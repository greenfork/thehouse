(import spork/misc)
(import ./log)
(use jaylib)
(use ./globals)
(use ./vector)

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
    :collision-cb (fn [self] (flicker self :magenta))
    :draw (fn [self] (draw-rectangle ;(self :pos) ;(self :dims) (self :color)))})
(defn <block> [& pairs] (misc/make Block ;pairs))
(defn <door> [& pairs] (misc/make Block :color :brown ;pairs))
(defn <exit-door> [& pairs]
  (misc/make Block :color :brown :type :exit-door ;pairs))

(def SpecialBlock (misc/make Block :active false))
(defn <one> [& pairs] (misc/make SpecialBlock :type :one ;pairs))
(defn <two> [& pairs] (misc/make SpecialBlock :type :two ;pairs))
(defn <three> [& pairs] (misc/make SpecialBlock :type :three ;pairs))

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
    :cell (+ :block :space :hero :door :exit-door :one :two :three)
    :row (* (some :cell) (? "\n"))
    :main (some :row)})

(defn- newline? [x] (= x (chr "\n")))
(def Level
  @{:ascii nil
    :hero nil
    :blocks nil
    :width (fn [self] (find-index newline? (self :ascii)))
    :height (fn [self] (inc (count newline? (self :ascii))))
    :screen-offset (fn [self sw sh]
                     [(math/round (- (/ sw 2) (* (/ (:width self) 2) block-side)))
                      (math/round (- (/ sh 2) (* (/ (:height self) 2) block-side)))])})
(defn- type? [& types] (fn [obj] (has-value? types (obj :type))))
(defn <level> [ascii]
  (def level (misc/make Level :ascii ascii))
  (def objects
    (->>
      ascii
      (peg/match level-grammar)
      (map |(unitize-obj $ block-side))
      (map |(apply-offset $ (:screen-offset level screen-width screen-height)))))
  (put level :hero (find (type? :hero) objects))
  (put level :blocks (filter (type? :block :exit-door :one :two :three) objects))
  level)

# Helpers

(defn- self-destroy-cb [col]
  (fn [self] (array/remove col (find-index |(= ($ :id) (self :id)) col))))
(defn- open-exit-doors-cb [col]
  (fn [self]
    (each ed (filter (type? :exit-door) col)
      (array/remove col (find-index |(= ($ :id) (ed :id)) col)))))
(defn- reset-collision-cb [obj] (set (obj :collision-cb) (fn [self])))
(defn- set-change-color-cb [type color col]
  (def obj (find (type? type) col))
  (set (obj :collision-cb)
       (fn [self]
         (log/debug* type "active")
         (change-color self color)
         (set (self :active) true)
         (reset-collision-cb self)
         (when (all |($ :active) (filter (type? :one :two :three) col))
           ((open-exit-doors-cb col) self)))))

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

(def hallway (<level> hallway-ascii))
(each exit-door (filter (type? :exit-door) (hallway :blocks))
  (set (exit-door :collision-cb) (open-exit-doors-cb (hallway :blocks))))

############
# Corridor #
############

(def corridor-ascii ``
BBBBBBBBBBBBBBBBBBBBB
D@..................d
D...................d
BBBBBBBBBBBBBBBBBBBBB
``)

(def corridor (<level> corridor-ascii))
(each exit-door (filter (type? :exit-door) (corridor :blocks))
  (set (exit-door :collision-cb) (open-exit-doors-cb (corridor :blocks))))

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

(def touch-the-stone (<level> touch-the-stone-ascii))
(set-change-color-cb :one :red (touch-the-stone :blocks))
(set-change-color-cb :two :green (touch-the-stone :blocks))
(set-change-color-cb :three :blue (touch-the-stone :blocks))
