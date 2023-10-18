(import spork/misc)
(import ./collision)
(import ./levels)
(import ./log)
(use jaylib)
(use judge)
(use ./globals)
(use ./vector)

(def levels [levels/hallway levels/corridor levels/touch-the-stone])
(def hero-vel (u 0.5))
(def vel-diag-mult (math/sin (/ math/pi 4)))

(def game
  @{:levels levels
    :cur-level-idx 0
    :must-exit? false
    :state :init})

(defn current-level [game] (in (game :levels) (game :cur-level-idx)))
(defn next-level! [game]
  (def cur-level-name ((current-level game) :name))
  (if (= (++ (game :cur-level-idx)) (length (game :levels)))
    (do
      (log/info* :next-level! true :exit true)
      (set (game :must-exit?) true))
    (log/info* :next-level true :from cur-level-name :to ((current-level game) :name))))
(defn maybe-exit-level [bb w h off]
  (def [[lx1 ly1] [lx2 ly2]] [off (v+ off [w h])])
  (def [[hx1 hy1] [hx2 hy2]] bb)
  # (log/trace* :maybe-exit-level true
  #             :off-min off
  #             :off-max (v+ off [w h])
  #             :bb-min (bb 0)
  #             :bb-max (bb 1))
  (when (or (< hx1 lx1) (< hy1 ly1) (> hx2 lx2) (> hy2 ly2))
    (next-level! game)))
(defn change-state [game new-state]
  (log/info* :change-state true :from (game :state) :to new-state)
  (set (game :state) new-state))

(defn run-text [texts]
  (when (key-pressed? :space)
    (change-state game :levels))
  (begin-drawing)
  (draw-text (first (texts "START")) (text-screen-offset 0) (text-screen-offset 1) 28 :ray-white)
  (end-drawing))

(defn execute-level-logic [level]
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

(def text-grammar
  ~{:title (* "." (<- (some (range "AZ"))) "\n\n")
    :single-text (* (not :title) (<- (some (if-not "\n\n" 1))) (? "\n\n"))
    :many-texts (group (some :single-text))
    :entry (* :s* :title :many-texts)
    :main (cmt (some :entry) ,(fn [& entries]
                                (->>
                                  entries
                                  (partition 2)
                                  (from-pairs))))})
(defn <text> [file-name]
  (->>
    (string "./assets/texts/" file-name ".txt")
    (slurp)
    (peg/match text-grammar)
    (first)))
(def start-text (<text> "start"))
(def hallway-text (<text> "hallway"))
(def corridor-text (<text> "corridor"))
(def touch-the-stone-text (<text> "touch_the_stone"))
(def dance-on-the-floor-text (<text> "dance_on_the_floor"))

(defn main
  [& args]
  (setdyn :log-level 0)

  (init-window screen-width screen-height "The House")
  (set-target-fps 60)
  (hide-cursor)

  (while (and (not (game :must-exit?)) (not (window-should-close)))
    (case (game :state)
      :init (run-text start-text)
      :levels (execute-level-logic (current-level game))))

  (close-window))
