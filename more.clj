(ns more
  (:use clojure.test))
  
(defn my-count
  [lst]
  (if (empty? lst)
    0
    (inc (my-count (rest lst)))))
    
(defn !
  [n]
  (loop [n	n
  	 accum	1]
    (if (zero? n)
      accum
      (recur (dec n) (* n accum)))))
      
(defn fibo
  "Computes the n-th therm in the fibonacci sequence."
  [n]
  (loop [n n
  	 a 0
  	 b 1]
  (if (zero? n)
    a
    (recur (dec n) b (+ a b)))))
  ;(if (<= n 1)
  ;  n
  ;  (+ (fibo (- n 1))
  ;     (fibo (- n 2)))))
    
(defn duplicate
  "Duplicates each element in list."
  [lst]
  (loop [lst 	lst
  	 result ()]
    (if (empty? lst)
      (reverse result)
      (let [x (first lst)]
        (recur (rest lst) (conj (conj result x) x))))))  
;  (if (empty? lst)
;    ()
;    (let [x (first lst)]
;    (conj (conj (duplicate (rest lst)) x) x))))

(defn my-map
  [fun lst]
  (if (empty? lst)
    ()
    (conj (my-map fun (rest lst)) (fun (first lst)))))


  
(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))
  
(deftest test-fibo
  (is (= 0 (fibo 0)))
  (is (= 1 (fibo 1)))
  (is (= 1 (fibo 2)))
  (is (= 2 (fibo 3)))
  (is (= 3 (fibo 4)))
  (is (= 5 (fibo 5)))
  (is (= 8 (fibo 6)))
  (is (= 13 (fibo 7))))
  
(deftest test-duplicate
  (is (= () (duplicate ())))
  (is (= '(1 1) (duplicate '(1))))
  (is (= '(1 1 2 2) (duplicate '(1 2))))
  (is (= '(1 1 2 2 3 3) (duplicate '(1 2 3))))
  (is (= '(1 1 2 2 3 3 4 4) (duplicate '(1 2 3 4))))
  (is (= '(1 1 2 2 3 3 4 4 5 5) (duplicate '(1 2 3 4 5)))))
  
 
(run-tests)
