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
  @{:id nil
    :type :hero
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
(defn change-color [obj color]
  (set (obj :color) color))

(def block-side (u 5))
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

(defn apply-offset [x map-offset]
  (v+= (x :pos) map-offset)
  x)
(defn unitize-obj [x size]
  (v*= (x :pos) [size size])
  x)

(var level-object-counter 0)
(defn lin-col [patt] ~(* (line) (column) ,patt))
(defn level-object
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

(def level1-ascii ``
..........BBBBBB
BBBBBBBBBBB....d
D@.............d
BBBBBBBBBBB....d
..........BBBBBB
``)

(def level2-ascii ``
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

(def level1 (<level> level1-ascii))
(defn destroy-cb [col]
  (fn [self] (array/remove col (find-index |(= ($ :id) (self :id)) col))))
(defn open-exit-doors-cb [col]
  (fn [self]
    (each ed (filter (type? :exit-door) col)
      (array/remove col (find-index |(= ($ :id) (ed :id)) col)))))
(each exit-door (filter (type? :exit-door) (level1 :blocks))
  (set (exit-door :collision-cb) (open-exit-doors-cb (level1 :blocks))))

(def level2 (<level> level2-ascii))
(defn set-change-color-cb [type color col]
  (def obj (find (type? type) col))
  (set (obj :collision-cb)
       (fn [self]
         (change-color self color)
         (set (self :active) true)
         (when (all |($ :active) (filter (type? :one :two :three) col))
           ((open-exit-doors-cb col) self)))))
(set-change-color-cb :one :red (level2 :blocks))
(set-change-color-cb :two :green (level2 :blocks))
(set-change-color-cb :three :blue (level2 :blocks))

(def levels [level1 level2])

(def game
  @{:levels levels
    :cur-level-idx 0
    :must-exit? false})

(defn next-level! [game]
  (if (= (++ (game :cur-level-idx)) (length (game :levels)))
    (set (game :must-exit?) true)))
(defn maybe-exit-level [bb w h off]
  (def [[lx1 ly1] [lx2 ly2]] [off (v+ off [w h])])
  (def [[hx1 hy1] [hx2 hy2]] bb)
  # (log/trace* :maybe-exit-level true
  #             :off-min off
  #             :off-max (v+ off [w h])
  #             :bb-min (bb 0)
  #             :bb-max (bb 1))
  (when (or (< hx1 lx1) (< hy1 ly1) (> hx2 lx2) (> hy2 ly2))
    (log/info "Exited current level")
    (next-level! game)))

(defn main
  [& args]
  (setdyn :log-level 0)

  (init-window screen-width screen-height "The House")
  (set-target-fps 60)
  (hide-cursor)

  (while (and (not (game :must-exit?)) (not (window-should-close)))
    (def level (in (game :levels) (game :cur-level-idx)))
    (def hero (level :hero))

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

      (each block (collision/filter-collided (:bb hero) (level :blocks))
        (when (collision/correct-coord (:bb hero) (:bb block))
          (:collision-cb block)))

      # Need to recalculate collided blocks because callbacks above could
      # remove them.
      (collision/correct-hero-position
        hero (collision/filter-collided (:bb hero) (level :blocks)) movev)

      (maybe-exit-level (:bb hero)
                        (* (:width level) block-side)
                        (* (:height level) block-side)
                        (:screen-offset level screen-width screen-height)))

    (each block (level :blocks) (:draw block))
    (:draw hero)

    (ev/sleep 0.001)
    (end-drawing))

  (close-window))
