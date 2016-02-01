;;;----------------------------------------------------
;;; ITESM CEM, November 11, 2011.
;;; Clojure Source File
;;; Activity: Magic Square
;;; Author: Marco Chávez
;;;----------------------------------------------------

(ns magic
  (:use 
    [clojure.pprint :only (cl-format)]
    [clojure.contrib.str-utils2 :only (repeat) :rename {repeat str-repeat}]))

(defn- magic-constant
  [n]
  (* n (/ (inc (* n n)) 2)))

(defn- magic-row?
  [square m f]	
  (apply =
    (cons m
      (f #(apply + %) square))))

(defn- rotate-square
  [square square-rotated i f]
  (if (< i 0)
    square-rotated
    (rotate-square
      square
      (conj square-rotated (f #(nth %1 i) square))
      (dec i)
      f)))

(defn- magic-diagonal?
  [square m n f]
  (let [lstdiag (range 0 n)
       rsquare (reverse square)]
  (and
   (= m (reduce + (f #(nth %1 %2) square lstdiag)))
   (= m (reduce + (f #(nth %1 %2) rsquare lstdiag))))))
  
(defn- magic?
  [lstsquare f]
  (let [n	(Math/sqrt (count lstsquare))
  	square  (partition n lstsquare)
  	m       (magic-constant n)
  	rsquare (rotate-square square () (dec n) f)]
  (and 
    (magic-row? square m f)
    (magic-row? rsquare m f)
    (magic-diagonal? square m n f))))

(defn- prnt-lines
  [n]
  (print"+----")
  (if(<= n 2)
    (println "+----+")
    (prnt-lines (dec n))))

(defn- prnt-numbers
  [lst]
  (if(empty? lst)
    (println "|")
    (do
      (print (cl-format nil (if (>= (first lst) 10) "|  ~:@<~A~>" "|   ~:@<~A~>" ) (first lst)))
      (prnt-numbers (rest lst)))))

(defn- prnt-square
  [square n]
  (loop [i    n
  	 lst  square
  	 n    n]
    (if (empty? lst)
      (prnt-lines n)
      (do
        (prnt-lines n)
        (prnt-numbers (first lst))
        (recur (dec n) (rest lst) n)))))

(defn- output-magic-square
  [square n]
  (let [m (magic-constant n)]
  (println (cl-format nil "The sum of all rows, all columns, and both diagonals is ~:@<~A~>." m))
  (prnt-square (partition n square) n)
  ))

(defn- create-squares
  [square setnum n end f]
  (if @end
    ()
  (if (empty? setnum)
    (if (magic? square f)
       (do
       	 (swap! end not)
       	 square)
      ;(output-magic-square square n end)
      ())
    (if (zero? (mod (count square) n))
      (if (magic-row? (partition n square) (magic-constant n) f)
        (apply concat (f #(create-squares (cons % square) (disj setnum %) n end f) setnum))
        ())
      (apply concat (map #(create-squares (cons % square) (disj setnum %) n end f) setnum))
      ))))

(defn- find-square
  [n f]
  (let [end (atom false)]
  (output-magic-square 
  (create-squares
    () 
    (set (range 1 (inc (* n n))))
    n
    end
    f)
    n)))

(defn square-seq
  [n]
  (find-square n map))

(defn square-par
  [n]
  (find-square n pmap))
