(defn v+ [a b] (map + a b))
(defn v+= [a b]
  (def sum (v+ a b))
  (set (a 0) (sum 0))
  (set (a 1) (sum 1))
  a)

(defn v- [a b] (map - a b))
(defn v-= [a b]
  (def diff (v- a b))
  (set (a 0) (diff 0))
  (set (a 1) (diff 1))
  a)

(defn v* [a b] (map * a b))
(defn v*= [a b]
  (def prod (v* a b))
  (set (a 0) (prod 0))
  (set (a 1) (prod 1))
  a)

(defn v/ [a b] (map / a b))
(defn v/= [a b]
  (def div (v/ a b))
  (set (a 0) (div 0))
  (set (a 1) (div 1))
  a)

(defn vsqrt [v]
  (map math/sqrt v))
(defn vsqrt* [v]
  (defn sign [n] (if (neg? n) -1 1))
  (map |(* ;((juxt (comp math/sqrt math/abs) sign) $)) v))

(defn vinvert [v] (map - v))

(defn v*m [v n] (map |(* n $) v))
