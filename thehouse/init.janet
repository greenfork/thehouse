(use jaylib)
(use judge)
(import spork/misc)
(import ./log)
(use ./vector)
(import ./collision)

(def unit 10)
(defn u [x] (* unit x))
(def screen-width (u 140))
(def screen-height (math/round (* screen-width (/ 9 16))))

(def hero-side (u 5))
(def Hero
  @{:type :hero
    :pos @[0 0]
    :dims @[hero-side hero-side]
    :bb (fn [self] @[(self :pos) (v+ (self :dims) (self :pos))])
    :move (fn [self v] (v+= (self :pos) v))
    :draw (fn [self] (draw-rectangle ;(map math/round (self :pos)) ;(self :dims) :maroon))})
(defn <hero> [& pairs] (misc/make Hero ;pairs))
(def hero-vel (u 0.5))
(def vel-diag-mult (math/sin (/ math/pi 4)))

(defn flicker [obj color]
  (def cur-color (obj :color))
  (when (not= cur-color color)
    (set (obj :color) color)
    (ev/spawn
      (ev/sleep 0.2)
      (set (obj :color) cur-color))))

(def block-side (u 5))
(def Block
  @{:type :block
    :pos @[0 0]
    :dims @[block-side block-side]
    :color :yellow
    :bb (fn [self] @[(self :pos) (v+ (self :dims) (self :pos))])
    :collision-cb (fn [self] (flicker self :magenta))
    :draw (fn [self] (draw-rectangle ;(self :pos) ;(self :dims) (self :color)))})
(defn <block> [& pairs] (misc/make Block ;pairs))

(defn apply-offset [x map-offset]
  (v+= (x :pos) map-offset)
  x)
(defn unitize-obj [x size]
  (v*= (x :pos) [size size])
  x)

(defn lin-col [patt] ~(* (line) (column) ,patt))
(defn level-object
  ``Create an object from map assuming that the object has a `:pos` field.``
  [letter ctor]
  ~(cmt ,(lin-col letter) ,(fn [l c] (ctor :pos @[(dec c) (dec l)]))))
(def parse-level
  ~{:space "."
    :block ,(level-object "B" <block>)
    :hero ,(level-object "@" <hero>)
    :cell (+ :block :space :hero)
    :row (* (some :cell) (? "\n"))
    :main (some :row)})

(def level1 ``
.BBBBBBBBBBBBBBBBBBBB.
.B..................B.
.B..................B.
.B...BB.............B.
B....................B
B....................B
.B...B..............B.
.B..................B.
.B..@...............B.
.BBBBBBBBBBBBBBBBBBBB.
``)

(defn main
  [& args]
  (setdyn :log-level 0)

  (init-window screen-width screen-height "The House")
  (set-target-fps 60)
  (hide-cursor)

  (defn newline? [x] (= x (chr "\n")))
  (def level-width (find-index newline? level1))
  (def level-height (inc (count newline? level1)))
  (def map-offset [(math/round (- (/ screen-width 2) (* (/ level-width 2) block-side)))
                   (math/round (- (/ screen-height 2) (* (/ level-height 2) block-side)))])
  (def level
    (->>
      level1
      (log/debug "Map: \n%s")
      (peg/match parse-level)
      (map |(unitize-obj $ block-side))
      (map |(apply-offset $ map-offset))))

  (def hero (find |(= ($ :type) :hero) level))
  (:move hero [0 -1])
  (def blocks (filter |(= ($ :type) :block) level))

  (while (not (window-should-close))
    (ev/sleep 0.001)
    (begin-drawing)
    (clear-background [0 0 0])

    (def movev
      (as?-> [0 0] _
        (if (key-down? :right) (v+ [hero-vel 0] _) _)
        (if (key-down? :left) (v+ [(- hero-vel) 0] _) _)
        (if (key-down? :up) (v+ [0 (- hero-vel)] _) _)
        (if (key-down? :down) (v+ [0 hero-vel] _) _)
        (if (= 2 (count zero? _)) nil _)
        (if (= 0 (count zero? _)) (map |(* vel-diag-mult $) _) _)))
    (when movev
      (:move hero movev)

      (def collided-blocks
        (->> blocks
          (filter |(collision/correct-coord (:bb hero) (:bb $)))
          # Sorting makes "sliding" movement possible because collision
          # with several objects first accounts for the nearest one.
          (sorted-by |(collision/bb-distance (:bb $) (:bb hero)))))

      (each block collided-blocks
        (when (collision/correct-coord (:bb hero) (:bb block))
          (:collision-cb block)))
      (collision/correct-hero-position hero collided-blocks movev))

    (each block blocks (:draw block))
    (:draw hero)

    (end-drawing))

  (close-window))
