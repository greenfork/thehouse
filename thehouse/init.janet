(import spork/misc)
(import ./collision)
(import ./levels)
(import ./log)
(use jaylib)
(use judge)
(use ./globals)
(use ./vector)

(def levels [levels/level1 levels/level2])
(def hero-vel (u 0.5))
(def vel-diag-mult (math/sin (/ math/pi 4)))

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
