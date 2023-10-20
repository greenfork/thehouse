(use jaylib)

(def unit 10)
(defn u [x] (* unit x))
(def screen-width (u 140))
(def screen-height (math/round (* screen-width (/ 9 16))))
(def block-side (u 5))

(defn draw-rectangle-wires [x y w h color]
  (draw-line x y (+ x w) y color)
  (draw-line x y x (+ y h) color)
  (draw-line (+ x w) y (+ x w) (+ y h) color)
  (draw-line x (+ y h) (+ x w) (+ y h) color))

(defn clump [n col]
  (var rs @[])
  (var idx 0)
  (while (< (+ idx n -1) (length col))
    (array/push rs (slice col idx (+ idx n)))
    (++ idx))
  rs)