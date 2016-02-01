(ns funs)

(defn sum
  ([] 0)
  ([n] n)
  ([n & more] (+ n (apply sum more))))
  
  ;[x & y]
  ;(if (empty? y)
  ;  x
  ;  (+ x (apply sum y))))
  
(defn compose
  [f g]
  (fn [x] (f (g x))))
  
(defn bla
  [n & more]
  more)
  
(defn f1 [x] (* x x))
(defn f2 [x] (+ x 3))
(def f3 (compose f1 f2))
(def f4 (compose f2 f1))
(def f5 (compose f3 f4))
