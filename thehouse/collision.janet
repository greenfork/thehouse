(use judge)
(use ./vector)
(import ./log)

(defn bb-distance [[amin amax] [bmin bmax]]
  (defn center [[x1 y1] [x2 y2]] [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])
  (def [acenter-x acenter-y] (center amin amax))
  (def [bcenter-x bcenter-y] (center bmin bmax))
  (def x-dist (math/abs (- acenter-x bcenter-x)))
  (def y-dist (math/abs (- acenter-y bcenter-y)))
  (math/sqrt (+ (* x-dist x-dist) (* y-dist y-dist))))
(test (bb-distance [[0 0] [2 2]] [[2 0] [4 2]]) 2)

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

(defn correct-coord
  ``Receive two bounding boxes with shapes of `[[x1 y1] [x2 y2]]` where
  `[x1 y1]` is the left-top position, return `[x! y!]` with a minimum
  correction for the position or nil otherwise.``
  [[[mx1 my1] [mx2 my2]] [[sx1 sy1] [sx2 sy2]]]
  (let [xcorr (correct-line [mx1 mx2] [sx1 sx2])
        ycorr (correct-line [my1 my2] [sy1 sy2])]
    (when (and xcorr ycorr)
      [xcorr ycorr])))

(defn correct-hero-position [hero block movev]
  (when-let [[xcorr ycorr] (correct-coord (:bb hero) (:bb block))]
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
    # (log/trace* :correction [xcorr ycorr]
    #             :compensation compensation
    #             :new-pos new-pos
    #             :movev movev)
    (set (hero :pos) new-pos)))

# Sorting makes "sliding" movement possible because collision
# with several objects first accounts for the nearest one.
(defn sort-by-bb-distance [target-bb blocks]
  (sorted-by |(bb-distance target-bb (:bb $)) blocks))
