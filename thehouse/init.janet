(use jaylib)
(use judge)
(import spork/misc)
(import ./log)
(use ./vector)

(def unit 10)
(defn u [x] (* unit x))
(def width (u 10))
(def height (u 10))
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

(defn bb-distance [[amin amax] [bmin bmax]]
  (defn center [[x1 y1] [x2 y2]] [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])
  (def [acenter-x acenter-y] (center amin amax))
  (def [bcenter-x bcenter-y] (center bmin bmax))
  (def x-dist (math/abs (- acenter-x bcenter-x)))
  (def y-dist (math/abs (- acenter-y bcenter-y)))
  (math/sqrt (+ (* x-dist x-dist) (* y-dist y-dist))))
(test (bb-distance [[0 0] [2 2]] [[2 0] [4 2]]) 2)

# If map is 10x10 blocks
(def map-offset [(math/round (- (/ screen-width 2) (* 5 block-side)))
                 (math/round (- (/ screen-height 2) (* 5 block-side)))])
(defn apply-offset [x]
  (v+= (x :pos) map-offset)
  x)
(defn unitize-obj [x]
  (v*= (x :pos) [block-side block-side])
  x)

(defn lin-col [patt] ~(* (line) (column) ,patt))
(def parse-map
  ~{:block (cmt ,(lin-col "B") ,(fn [l c] (<block> :pos @[(dec c) (dec l)])))
    :space "."
    :hero (cmt ,(lin-col "@") ,(fn [l c] (<hero> :pos @[(dec c) (dec l)])))
    :cell (+ :block :space :hero)
    :row (* (10 :cell) (? "\n"))
    :main (10 :row)})

(def level1 ``
BBBBBBBBBB
B........B
B........B
B...BB...B
B........B
B........B
B...B....B
B........B
B..@.....B
BBBBBBBBBB
``)

(defn correct-line
  ``Receive two lines with shapes of `[x1 x2]` and return the minimal
  correction for movable line X to not be intersected or nil otherwise.``
  [[x1 x2] [y1 y2]]
  (assert (>= x2 x1) (string/format "X line must be ascending: instead: %q" [x1 x2]))
  (assert (>= y2 y1) (string/format "Y line must be ascending: instead: %q" [y1 y2]))
  (def x1-y1 (- x1 y1))
  (def x1-y2 (- x1 y2))
  (def x2-y1 (- x2 y1))
  (def x2-y2 (- x2 y2))
  # (log/trace* :correct-line true :x1-y1 x1-y1 :x1-y2 x1-y2 :x2-y1 x2-y1 :x2-y2 x2-y2)
  (def result
    (cond
      # X is inside Y.
      (and (>= x1-y1 0) (<= x2-y2 0))
      (if (> (math/abs x1-y2) (math/abs x2-y1)) (- x2-y1) (- x1-y2))
      # X contains Y.
      (and (neg? x1-y1) (pos? x2-y2))
      (if (> (math/abs x1-y2) (math/abs x2-y1)) (- x2-y1) (- x1-y2))
      # X intersects Y from the left.
      (and (neg? x1-y1) (pos? x2-y1)) (- x2-y1)
      # X intersects Y from the right.
      (and (neg? x1-y2) (pos? x2-y2)) (- x1-y2)
      nil))
  (if (zero? result) nil result))
(test (correct-line [1 3] [2 6]) -1)
(test (correct-line [3 8] [2 6]) 3)
(test (correct-line [3 4] [2 6]) -2)
(test (correct-line [4 5] [2 6]) 2)
(test (correct-line [5 6] [2 6]) 1)
(test (correct-line [2 6] [3 4]) 2)
(test (correct-line [2 6] [4 5]) -2)
(test (correct-line [2 3] [3 4]) nil)
(test (correct-line [4 4] [3 4]) nil)
(test (correct-line [4 5] [3 4]) nil)

(defn correct-pos
  ``Receive two bounding boxes with shapes of `[[x1 y1] [x2 y2]]` where
  `[x1 y1]` is the left-top position, return `[x! y!]` with a minimum
  correction for the position or nil otherwise.``
  [[[mx1 my1] [mx2 my2]] [[sx1 sy1] [sx2 sy2]]]
  (let [xcorr (correct-line [mx1 mx2] [sx1 sx2])
        ycorr (correct-line [my1 my2] [sy1 sy2])]
    (when (and xcorr ycorr)
      [xcorr ycorr])))

(defn main
  [& args]
  (setdyn :log-level 0)

  (init-window screen-width screen-height "The House")
  (set-target-fps 60)
  (hide-cursor)

  (def level
    (->>
      level1
      (log/debug "Map: \n%s")
      (peg/match parse-map)
      (map unitize-obj)
      (map apply-offset)))

  (def hero (find |(= ($ :type) :hero) level))
  (:move hero [0 -1])
  (def blocks (filter |(= ($ :type) :block) level))

  (while (not (window-should-close))
    (ev/sleep 0.0001)
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
          (filter |(correct-pos (:bb hero) (:bb $)))
          # Sorting makes "sliding" movement possible because collision
          # with several objects first accounts for the nearest one.
          (sorted-by |(bb-distance (:bb $) (:bb hero)))))

      (each block collided-blocks
        (when (correct-pos (:bb hero) (:bb block))
          (:collision-cb block)))

      (each block collided-blocks
        (when-let [[xcorr ycorr] (correct-pos (:bb hero) (:bb block))]
          # When movement is only on one axis, compensate to this axis.
          # When movement is on both axes, choose the minimum compensation.
          (def compensation
            (match movev
              [0 0] (error "movev can't be [0 0], must be nil")
              [_ 0] [xcorr 0]
              [0 _] [0 ycorr]
              [_ _] (if (> (math/abs ycorr) (math/abs xcorr))
                      [xcorr 0]
                      [0 ycorr])))
          (def new-pos (v+ (hero :pos) compensation))
          (log/trace* :correction [xcorr ycorr]
                      :compensation compensation
                      :new-pos new-pos
                      :movev movev)
          (set (hero :pos) new-pos))))

    (each block blocks (:draw block))
    (:draw hero)

    (end-drawing))

  (close-window))
