(import spork/misc)
(import spork/netrepl)
(import ./collision)
(import ./levels)
(import ./log)
(import ./text)
(use jaylib)
(use judge)
(use ./globals)
(use ./vector)

(def hero-vel (u 0.5))
(def vel-diag-mult (math/sin (/ math/pi 4)))

(def game
  @{:cur-level-idx 0
    :frame 0
    :must-exit? false
    # Phases: init, levels, final
    :phase :init
    :state @{:init 0}})

(defn exit-game [game]
  (set (game :must-exit?) true))
(defn curstate [game]
  (case (game :phase)
    :init (in (game :state) (game :phase))
    :levels ((curlevel game) :state)))
(defn next-level! [game]
  (def cur-level-name ((curlevel game) :name))
  (if (= (++ (game :cur-level-idx)) (length (game :levels)))
    (do
      (log/info* :next-level true :exit true)
      (exit-game game))
    (log/info* :next-level true :from cur-level-name :to ((curlevel game) :name))))
(defn change-phase [game new-phase]
  (log/info* :change-phase true :from (game :phase) :to new-phase)
  (set (game :phase) new-phase))
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

(def levels [levels/hallway levels/corridor levels/touch-the-stone
             (levels/make-dance-on-the-floor) (levels/make-clean-me)
             (levels/make-final (fn [] (change-phase game :final)))])
(put game :levels levels)

(defn increase-frame-counter [game]
  (if (< (game :frame) math/int-max)
    (+= (game :frame) 1)
    (set (game :frame) 0)))

(defn call/not [call-frames not-frames f]
  (def total (+ call-frames not-frames))
  # (log/trace* :total total :mod (% (game :frame) total)
  #             :call-frames call-frames
  #             :condition (< (% (game :frame) total) call-frames))
  (when (< (% (game :frame) total) call-frames)
    (f)))

(defn draw-press-space []
  (def text "Press Space")
  (def pos [(math/round (- (/ screen-width 2) (/ (text/measure text) 2)))
            (- screen-height 50)])
  (call/not 120 60 (fn [] (text/draw text pos :size :small :color :light-gray))))

(defn run-text [texts]
  (begin-drawing)
  (clear-background [0 0 0])
  (var next-pos text-full-screen-offset)
  (for idx 0 (inc (curstate game))
    (set next-pos
         (text/draw (text/layout (idx (texts "START")) text-width) next-pos)))
  (draw-press-space)
  # (draw-rectangle-wires (text-full-screen-offset 0) (text-full-screen-offset 1)
  #                       text-width 600 :yellow)
  (end-drawing)

  (when (key-pressed? :space)
    (if (< (inc (curstate game)) 3)
      (update-in game [:state :init] inc)
      (change-phase game :levels))))

(defn execute-level-text [level]
  (def hero (level :hero))
  (begin-drawing)
  (clear-background [0 0 0])
  (each block (level :blocks) (:draw block))
  (:draw hero)
  (:run (levels/curstate level))
  (draw-press-space)
  (end-drawing)
  (when (key-pressed? :space)
    (levels/change-phase level (:advance (levels/curstate level)))))

(defn execute-level-default [level]
  (def hero (level :hero))
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

    (each block
      (filter (fn [b]
                (and (collision/correct-coord (:bb hero) (:bb b))
                     (:collision-cb b)))
              (collision/sort-by-bb-distance (:bb hero) (level :blocks)))
      (collision/correct-hero-position hero block movev))
    (maybe-exit-level (:bb hero)
                      (* (:width level) block-side)
                      (* (:height level) block-side)
                      (:screen-offset level screen-width screen-height)))

  (begin-drawing)
  (clear-background [0 0 0])
  (each block (level :blocks) (:draw block))
  (:draw hero)
  (end-drawing))

(defn main
  [& args]
  (setdyn :log-level 2)
  (set-config-flags :window-resizable :vsync-hint)

  # Start a repl to connect to.
  (with [netrepl-stream
         (netrepl/server-single
           "127.0.0.1" "9365" (curenv)
           # Exits game when client closes connection.
           (fn [stream] (exit-game game))
           "Welcome to The House\n")
         # Shuts down clients when game loop is exited.
         (fn [stream] (:close stream) (quit))]
    # Open and close window, necessary to have a destructor so that on error
    # the window closes. Otherwise it stays open because of the running REPL.
    (with [_
           (init-window screen-width screen-height "The House")
           (fn [_] (close-window))]
      (set-target-fps 60)
      (hide-cursor)
      (text/init)
      (while (and (not (game :must-exit?)) (not (window-should-close)))
        (increase-frame-counter game)
        (draw-fps 0 0)
        (case (game :phase)
          :init (run-text text/start-text)
          :levels (let [level (curlevel game)]
                    (case (level :phase)
                      :default (execute-level-default level)
                      (execute-level-text level)))
          :final (exit-game game))
        # Yield control to other fibers for things such as REPL and animation.
        (ev/sleep 0.001))
      (text/deinit))))
