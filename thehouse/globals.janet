(def unit 10)
(defn u [x] (* unit x))
(def screen-width (u 140))
(def screen-height (math/round (* screen-width (/ 9 16))))
(def block-side (u 5))
(def text-screen-offset
  [(math/round (* screen-width (/ 1 6)))
   (math/round (* screen-height (/ 1 3)))])
