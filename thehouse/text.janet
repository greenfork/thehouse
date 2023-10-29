(use jaylib)
(use ./globals)
(use ./vector)

(def fonts @{:small nil :normal nil})
(def sizes {:small 40 :normal 56})
(def font-spacing 1)
(def line-height 1)
(def vertical-offset (* (sizes :normal) line-height))

(def font-ttf (slurp "./assets/fonts/Europeana_One.ttf"))

(defn init []
  (def ascii-range (range 32 127))
  (def cyrillic-range (range 0x410 0x450))
  (def font-range (array/concat ascii-range cyrillic-range))
  (set (fonts :normal) (load-font-from-memory
                         ".ttf" font-ttf (length font-ttf)
                         (sizes :normal) font-range))
  (set (fonts :small) (load-font-from-memory
                        ".ttf" font-ttf (length font-ttf)
                        (sizes :small) font-range)))
(defn deinit []
  (unload-font (fonts :normal))
  (unload-font (fonts :small)))

(defn draw
  ``Draw some `text` at `pos` position, moving to the next line on each `"\n"`
  newline character. Returns the position of the next line after the drawn text``
  [text pos &named size color]
  (default size :normal)
  (default color :ray-white)
  (def font-size (sizes size))
  (def font (fonts size))
  (assert (not (nil? font)) "FONT is not set")
  (def text_and_pos
    (as-> text _
      (string/split "\n" _)
      (map (fn [t i]
             [t (v+ pos [0 (* vertical-offset i)])])
           _
           (range (length _)))))
  (each [text pos] text_and_pos
    (draw-text-ex font text pos font-size font-spacing color))
  (->>
    text_and_pos
    (last)
    (1)
    (v+ [0 vertical-offset])))

(defn measure [text]
  (assert (not (nil? (fonts :normal))) "FONT is not set")
  (0 (measure-text-ex (fonts :normal) text (sizes :normal) font-spacing)))

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
  ~{:double-newline (+ "\n\n" "\r\n\r\n")
    :title (* "." (<- (some (+ (range "AZ") (range "09") (set "-")))) :double-newline)
    :single-text (* (not :title) (<- (some (if-not :double-newline 1))) (? :double-newline))
    :many-texts (group (some :single-text))
    :entry (* :s* :title :many-texts)
    :main (cmt (some :entry) ,(fn [& entries]
                                (->>
                                  entries
                                  (partition 2)
                                  (map (fn [[section text]]
                                         [section (map |(peg/replace-all :s+ " " $) text)]))
                                  (from-pairs))))})
(defn- <text> [file-name language]
  (def path
    (case language
      :ru "./assets/texts/ru/"
      :en "./assets/texts/en/"))
  (->>
    (string path file-name ".txt")
    (slurp)
    (peg/match text-grammar)
    (first)))

(def- start-text/en (<text> "start" :en))
(def- hallway-text/en (<text> "hallway" :en))
(def- corridor-text/en (<text> "corridor" :en))
(def- touch-the-stone-text/en (<text> "touch_the_stone" :en))
(def- dance-on-the-floor-text/en (<text> "dance_on_the_floor" :en))
(def- clean-me-text/en (<text> "clean_me" :en))
(def- final-text/en (<text> "final" :en))

(def- start-text/ru (<text> "start" :ru))
(def- hallway-text/ru (<text> "hallway" :ru))
(def- corridor-text/ru (<text> "corridor" :ru))
(def- touch-the-stone-text/ru (<text> "touch_the_stone" :ru))
(def- dance-on-the-floor-text/ru (<text> "dance_on_the_floor" :ru))
(def- clean-me-text/ru (<text> "clean_me" :ru))
(def- final-text/ru (<text> "final" :ru))

(defn get [name]
  (def language (or (dyn :language) :en))
  (match [name language]
    ["start" :en] start-text/en
    ["start" :ru] start-text/ru
    ["hallway" :en] hallway-text/en
    ["hallway" :ru] hallway-text/ru
    ["corridor" :en] corridor-text/en
    ["corridor" :ru] corridor-text/ru
    ["touch-the-stone" :en] touch-the-stone-text/en
    ["touch-the-stone" :ru] touch-the-stone-text/ru
    ["dance-on-the-floor" :en] dance-on-the-floor-text/en
    ["dance-on-the-floor" :ru] dance-on-the-floor-text/ru
    ["clean-me" :en] clean-me-text/en
    ["clean-me" :ru] clean-me-text/ru
    ["final" :en] final-text/en
    ["final" :ru] final-text/ru))
