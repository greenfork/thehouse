(use jaylib)
(use ./globals)
(use ./vector)

(var FONT nil)

(def size 56)
(def spacing 0.5)
(def line-height 1)
(def vertical-offset (* size line-height))
(def font-ttf (slurp "./assets/fonts/Europeana_One.ttf"))

(defn init []
  (set FONT (load-font-from-memory ".ttf" font-ttf (length font-ttf)
                                   size (range 32 127))))
(defn deinit []
  (unload-font FONT))

(defn draw
  ``Draw some `text` at `pos` position, moving to the next line on each `"\n"`
  newline character. Returns the position of the next line after the drawn text``
  [text pos &opt color]
  (assert (not (nil? FONT)) "FONT is not set")
  (default color :ray-white)
  (def text_and_pos
    (as-> text _
      (string/split "\n" _)
      (map (fn [t i]
             [t (v+ pos [0 (* vertical-offset i)])])
           _
           (range (length _)))))
  (each [text pos] text_and_pos
    (draw-text-ex FONT text pos size spacing color))
  (->>
    text_and_pos
    (last)
    (1)
    (v+ [0 vertical-offset])))

(defn measure [text]
  (assert (not (nil? FONT)) "FONT is not set")
  (0 (measure-text-ex FONT text size spacing)))

(defn layout
  ``Insert newline characters `"\n"` in the `text` where the text would exceed
  `max-width` pixels.``
  [text max-width]
  (def words (string/split " " text))
  (def word-widths (map measure words))
  # Two spaces minus one space because solely single space may be shorter.
  (def space-len (- (measure "  ") (measure " ")))
  (var width-sum 0)
  (var idx 0)
  (var slice-indices @[0])
  (while (< idx (length word-widths))
    (def len (idx word-widths))
    # (log/trace* :sum width-sum :len len :max max-width)
    (if (< (+ width-sum len) max-width)
      (+= width-sum len space-len)
      (do
        (set width-sum len)
        (array/push slice-indices idx)))
    (++ idx))
  (array/push slice-indices -1)
  # (log/trace* :widths (map math/round word-widths) :sp (math/round space-len) :sl slice-indices)
  # (error "hey")
  (as->
    slice-indices _
    (clump 2 _)
    (map (fn [[start end]]
           (as-> (array/slice words start end) $
             (string/join $ " "))) _)
    (string/join _ "\n")))

###############
# Load assets #
###############

(def- text-grammar
  ~{:title (* "." (<- (some (range "AZ"))) "\n\n")
    :single-text (* (not :title) (<- (some (if-not "\n\n" 1))) (? "\n\n"))
    :many-texts (group (some :single-text))
    :entry (* :s* :title :many-texts)
    :main (cmt (some :entry) ,(fn [& entries]
                                (->>
                                  entries
                                  (partition 2)
                                  (map (fn [[section text]]
                                         [section (map |(peg/replace-all :s+ " " $) text)]))
                                  (from-pairs))))})
(defn- <text> [file-name]
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
