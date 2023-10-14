(use jaylib)
(import spork/misc)
(import ./log)

(defn v+ [a b] (map + a b))
(defn v+= [a b]
  (def sum (v+ a b))
  (set (a 0) (sum 0))
  (set (a 1) (sum 1))
  a)
(defn v* [a b] (map * a b))
(defn v*= [a b]
  (def prod (v* a b))
  (set (a 0) (prod 0))
  (set (a 1) (prod 1))
  a)
(defn vsqrt [v]
  (map math/sqrt v))
(defn vsqrt* [v]
  (defn sign [n] (if (neg? n) -1 1))
  (map |(* ;((juxt (comp math/sqrt math/abs) sign) $)) v))

(def unit 10)
(defn u [x] (* unit x))
(def width (u 10))
(def height (u 10))
(def screen-width (u 140))
(def screen-height (math/round (* screen-width (/ 9 16))))
(def block-side (u 5))

(def Hero
  @{:type :hero
    :pos @[0 0]
    :dims @[block-side block-side]
    :move (fn [self v] (v+= (self :pos) (log/trace "Move: %q" v)))
    :draw (fn [self] (draw-rectangle ;(map math/round (self :pos)) ;(self :dims) :maroon))})
(defn <hero> [& pairs] (misc/make Hero ;pairs))
(def hero-vel (u 0.5))
(def vel-diag-mult (math/sin (/ math/pi 4)))

(def Block
  @{:type :block
    :pos @[0 0]
    :dims @[block-side block-side]
    :draw (fn [self] (draw-rectangle ;(self :pos) ;(self :dims) :yellow))})
(defn <block> [& pairs] (misc/make Block ;pairs))

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

(def level1 ```
BBBBBBBBBB
B........B
B........B
B........B
B........B
B........B
B........B
B........B
B..@.....B
BBBBBBBBBB
```)

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
  (def blocks (filter |(= ($ :type) :block) level))

  (while (not (window-should-close))
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
      (:move hero movev))

    (each x blocks (:draw x))
    (:draw hero)

    (end-drawing))

  (close-window))
