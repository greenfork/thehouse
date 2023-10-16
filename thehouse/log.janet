(defn log [level str-level fmt & xs]
  (when (>= level (dyn :log-level))
    (eprintf
      (string "%s [%s] " fmt)
      str-level (os/strftime "%Y-%m-%d %H:%M:%S") ;xs)))
(defn trace [fmt & xs] (log 0 "\e[35mT\e[0m" fmt ;xs) (xs 0))
(defn debug [fmt & xs] (log 1 "\e[36mD\e[0m" fmt ;xs) (xs 0))
(defn info [fmt & xs] (log 2 "\e[32mI\e[0m" fmt ;xs) (xs 0))
(defn error [fmt & xs] (log 3 "\e[31mE\e[0m" fmt ;xs) (xs 0))
(defn trace* [& pairs]
  (assert (even? (length pairs)) "Arguments must be even")
  (def pairs (partition 2 pairs))
  (defn fmt [pairs] (map |(string/format "%s=%%q" ($ 0)) pairs))
  (defn vals [pairs] (map |($ 1) pairs))
  (trace (string/join (fmt pairs) " ") ;(vals pairs)))
