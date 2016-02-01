(ns seqs
  (:use clojure.test))
  
(defn enlist
  [lst]
  ;(map list lst))
  (for [e lst] (list e)))

(defn list-of-symbols?
  [lst]
  (every? symbol? lst))
  
(defn average
  [lst]
  ;(/ (apply + lst) (count lst)))
  (/ (reduce + lst) (count lst)))
  
(defn fact
  [n]
  (reduce * (range 1 (inc n))))
  
(defn invert-pairs
  [lst]
  ;(map reverse lst))
  (map #(vector (% 1) (% 0)) lst))
  
(defn compress
  [lst]
  (map first (partition-by identity lst)))
  
(defn pack
  [lst]
  (partition-by identity lst))
  
(defn encode
  [lst]
  (for [e (pack lst)] [(count e) (first e)]))
	  
(defn encode-modified
  [lst]
  (map #(if (= 1 (% 0)) (% 1) %) (encode lst)))
  
(defn decode
  [lst]
  (apply 
    concat
    (for [i lst]
    (if (vector? i)
      (repeat (i 0) (i 1))
      (list i)))))
  
(deftest test-enlist
  (is (= () (enlist '())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))
  
(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-average
  (is (= 5.0 (average '(5.0))))
  (is (= 5.12 (average '(3.1 4.2 7.3 9.8 1.2))))
  (is (thrown? ArithmeticException (average ()))))
  
(deftest test-fact
  (is (= 1 (fact 0)))
  (is (= 1 (fact 1)))
  (is (= 2 (fact 2)))
  (is (= 6 (fact 3)))
  (is (= 120 (fact 5)))
  (is (= 3628800 (fact 10)))
  (is (= 265252859812191058636308480000000 (fact 30))))
  
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March]) 
         (invert-pairs '([January 1][February 2][March 3])))))
         
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))
  
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))
  
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))
  
(run-tests)
