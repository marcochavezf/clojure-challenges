(ns euler)

(defn- make-sequence
  [f n]
  (lazy-seq (cons n (make-sequence f (f n)))))

(defn- sum-of-seq-before-than
  "Returns the sum of a lazy sequence f, where n is the stop number."
  [n lazy]
  (reduce + (take-while #(< % n) lazy)))

;;;;;;;;;;;;;;;;;;;;
;;Problem 1
;;;;;;;;;;;;;;;;;;;;

(defn three-five-seq
  []
  (let [lst (make-sequence inc 1)]
  (filter #(or (zero? (mod % 3)) (zero? (mod % 5))) lst)))

;;;;;;;;;;;;;;;;;;;;
;;Problem 2
;;;;;;;;;;;;;;;;;;;;

(defn- fibo-seq
  [ant sig]
  (lazy-seq (cons ant (fibo-seq sig (+ ant sig)))))

(defn fibo-even-seq
  []
  (filter #(zero? (mod % 2)) (fibo-seq 1 2)))

;;;;;;;;;;;;;;;;;;;;
;;Problem 3
;;;;;;;;;;;;;;;;;;;;

(defn primes-factors-aux
  [n]
  (filter #(zero? (mod n %)) (make-sequence inc 1)))
  ;(filter #(zero? (mod n %)) (take-while #(< % n) (make-sequence inc 1))))

(defn primes-factors
  [lst]
  (loop [seq ()
  	 lst  (rest lst)]
    (if (empty? lst)
      seq
      (recur  
      	(cons (first lst) seq) 
        (remove #(zero? (mod % (first lst))) (rest lst))))))

;;;;; Solutions
(defn s1
  []
  (sum-of-seq-before-than 1000 (three-five-seq)))

(defn s2
  []
  (sum-of-seq-before-than 4000000 (fibo-even-seq)))

(defn s3
  []
  (first (primes-factors (primes-factors-aux 600851475143))))
