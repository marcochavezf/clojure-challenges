
(defn f
  [x]
  (println x)
  15)
  
(defn g
  [lst]
  (map inc lst))
  
(defn make-naturals
  [n]
  (lazy-seq (cons n (make-naturals (inc n)))))
  
  
(defn primes-aux
  [lst]
  (lazy-seq 
    (cons 
      (first lst) 
      (primes-aux
       (remove 
         #(zero? (rem % (first lst))) 
         (rest lst))))))
(defn primes
  []
  (primes-aux (iterate inc 2)))
