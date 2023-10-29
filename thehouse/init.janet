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
    # Phases: menu, init, levels, final
    :phase :menu
    :state @{:init 0
             :menu :play}})
(defn exit-game [game]
  (set (game :must-exit?) true))
(defn curstate [game]
  (case (game :phase)
    :menu (in (game :state) (game :phase))
    :init (in (game :state) (game :phase))
    :levels ((curlevel game) :state)))
(defn setstate [game val]
  (case (game :phase)
    :menu (put-in game [:state :menu] val)
    (error "Not implemented")))
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

# Lazily generate each level.
(defn game/init [game]
  (def levels [(delay (levels/make-hallway))
               (delay (levels/make-corridor))
               (delay (levels/make-touch-the-stone))
               (delay (levels/make-dance-on-the-floor))
               (delay (levels/make-clean-me))
               (delay (levels/make-final (fn [] (change-phase game :final))))])
  (put game :levels levels))


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
  (def text
    (case (dyn :language)
      :ru "Нажмите Пробел"
      "Press Space"))
  (def pos [(math/round (- (/ screen-width 2) (/ (text/measure text) 2)))
            (- screen-height 50)])
  (call/not 120 60 (fn [] (text/draw text pos :size :small :color :light-gray))))

(defn draw-menu []
  (def curstate (curstate game))
  (when (or (key-pressed? :enter) (key-pressed? :space))
    (case curstate
      :play (change-phase game :init)
      :play/ru (do
                 (setdyn :language :ru)
                 (change-phase game :init))
      :quit (change-phase game :final)
      :quit/ru (change-phase game :final)))
  (when (and (key-pressed? :right) (= curstate :play))
    (setstate game :play/ru))
  (when (and (key-pressed? :right) (= curstate :quit))
    (setstate game :quit/ru))
  (when (and (key-pressed? :left) (= curstate :play/ru))
    (setstate game :play))
  (when (and (key-pressed? :left) (= curstate :quit/ru))
    (setstate game :quit))
  (when (and (key-pressed? :down) (= curstate :play))
    (setstate game :quit))
  (when (and (key-pressed? :down) (= curstate :play/ru))
    (setstate game :quit/ru))
  (when (and (key-pressed? :up) (= curstate :quit))
    (setstate game :play))
  (when (and (key-pressed? :up) (= curstate :quit/ru))
    (setstate game :play/ru))
  (def init-offset [(/ screen-width 5) (/ screen-height 8)])
  (def vert-margin 60)
  (def title-margin 200)
  (def lang-margin 500)
  (defn color [state]
    (if (= state curstate) :yellow :ray-white))
  (begin-drawing)
  (clear-background [0 0 0])
  (text/draw "The House" init-offset :size :title)
  (text/draw "Play" (v+ init-offset [0 (+ title-margin vert-margin)]) :color (color :play))
  (text/draw "Quit" (v+ init-offset [0 (+ title-margin (* 2 vert-margin))]) :color (color :quit))
  (text/draw "Тот Дом" (v+ init-offset [lang-margin 0]) :size :title)
  (text/draw "Играть" (v+ init-offset [lang-margin (+ title-margin vert-margin)]) :color (color :play/ru))
  (text/draw "Выход" (v+ init-offset [lang-margin (+ title-margin (* 2 vert-margin))]) :color (color :quit/ru))
  (end-drawing))

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

(def music-src (slurp "./assets/music/dream_2.ogg"))

(defn main
  [& args]
  (setdyn :log-level 3)
  (set-config-flags :vsync-hint)
  (set-config-flags :window-resizable)

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
      (game/init game)
      (text/init)
      (init-audio-device)
      (def music (load-music-stream-from-memory ".ogg" music-src (length music-src)))
      (play-music-stream music)
      (while (and (not (game :must-exit?)) (not (window-should-close)))
        (increase-frame-counter game)
        (case (game :phase)
          :menu (draw-menu)
          :init (run-text (text/get "start"))
          :levels (let [level (curlevel game)]
                    (case (level :phase)
                      :default (execute-level-default level)
                      (execute-level-text level)))
          :final (exit-game game))
        (when (window-focused?)
          (update-music-stream music))
        # Yield control to other fibers for things such as REPL and animation.
        (ev/sleep 0.001))
      (text/deinit)
      (unload-music-stream music)
      (close-audio-device))))
