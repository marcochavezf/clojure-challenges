;;; ITESM CEM, September 17, 2011.
;;; Clojure Source File
;;; Activity: Recursive Functions, Part II
;;; Author: Marco Chávez    

(ns recursion2
  (:use clojure.test)
  (:use clojure.contrib.trace))

(defn my-repeat
  "Returns a list that contains n copies of x."
  [n x]
  (if (zero? n)
    ()
    (conj (my-repeat (dec n) x) x)))
    
(defn invert-pairs
  "Returns a new list with every vector pair inverted."
  [lst]
  (if (empty? lst)
    ()
    (conj (invert-pairs (rest lst)) (rseq (first lst)))))
    
(defn enlist
  "Surrounds in a list every upper-level element of the list it takes as input."
  [lst]
  (if (empty? lst)
    ()
    (let [elm 	(first lst)
     	 restlst   (rest lst)]
      (conj (enlist restlst) (conj () elm)))))
      
(defn my-interleave
  "Returns a list containing the first element of a, followed by the first element of b, followed by the second element of a, followed by the second element of b, and so on."
  [a b]
  (if (or (empty? a) (empty? b))
    ()
    (conj (conj (my-interleave (rest a) (rest b)) (first b)) (first a))))
    
(defn my-flatten
  "Removes all the interior parenthesis of the list it takes as input."
  [lst]
  (if (list? lst)
    (if (empty? lst)
      ()
      (concat (my-flatten (first lst)) (my-flatten (rest lst))))
  (conj () lst)))
    
(defn exchange
  "Returns a list with the same elements as lst, except that all occurrences of x1 are replaced by x2 and vice versa, including any occurrences inside nested lists."
  [x1 x2 lst]
  (if (list? lst)
    (if (empty? lst)
      ()
      (conj (exchange x1 x2 (rest lst)) (exchange x1 x2 (first lst))))
   (if (= lst x1)
     x2
     (if (= lst x2)
       x1
       lst))))
       
(defn insert
  "Returns a new list with the same elements as lst but inserting n in its corresponding place."
  [n lst]
  (if (empty? lst)
    (conj () n)
    (if (< n (first lst))
      (conj lst n)
      (conj (insert n (rest lst)) (first lst)))))

(defn my-sort
  "Returns a new list with the same elements but in ascending order."
  [lst]
  (if (empty? lst)
    ()
    (insert (first lst) (my-sort (rest lst)))))

(defn binary
  "If n is equal to zero, it returns an empty list. If n is greater than zero, it returns a list with a sequence of ones and zeros equivalent to the binary representation of n."
  [n]
  (if (zero? n)
    ()
    (reverse (conj (reverse (binary (quot n 2))) (rem n 2)))))
    
(defn search-prime-factor
  "Searches the first prime factor of a number"
  [n]
  (loop[n      n
  	factor 2]
  	(if (= (rem n factor) 0)
  	  factor
  	  (recur n (inc factor)))))

(defn prime-factors
  "Returns a list containing the prime factors of n in ascending order."
  [n]
  (if (= n 1)
    ()
    (let [factor (search-prime-factor n)]
    	 (conj (prime-factors (quot n factor)) factor))))
   
(defn compress
  "Takes a list lst as its argument. If lst contains consecutive repeated elements, they should be replaced with a single copy of the element."
  [lst]
  (if (empty? lst)
    ()
    (let [fe   (first lst)
    	  rlst (rest lst)]
      (if (= fe (first rlst))
        (compress rlst)
        (conj (compress rlst) fe)))))

(defn pack-tail
  "Returns a sublist of the first element and calls to pack function with the rest of the list as its argument"
  [lst f]
  (loop [fe   	 (first lst)
    	 rlst 	 (rest lst)
    	 package (conj () fe)]
    	 
    	 (let [se (first rlst)]
    	 
    	 (if (= fe se)
    	   (recur se
    	   	  (rest rlst)
    	   	  (conj package fe))
    	   (conj (f rlst) package)))))

(defn pack
  "Takes a list lst as its argument. If lst contains consecutive repeated elements they should be placed in separate sublists."
  [lst]
  (if (empty? lst)
    lst
    (pack-one-letter lst pack)))

(defn encode-one-letter
  [lst f]
  (loop [fe   	 (first lst)
    	 rlst 	 (rest lst)
    	 ne 	 1]
    	 
    	 (let [se (first rlst)]
    	 
    	 (if (= fe se)
    	   (recur se
    	   	  (rest rlst)
    	   	  (inc ne))
    	   (conj (f rlst) (vector ne fe))))))
    
(defn encode
  "Takes a list lst as its argument. Consecutive duplicates of elements in lst are encoded as vectors [n e], where n is the number of duplicates of the element e."
  [lst]
  (if (empty? lst)
    lst
    (encode-one-letter lst encode)))
    
(defn encode-one-letter-modified
  [lst f]
  (loop [fe   	 (first lst)
    	 rlst 	 (rest lst)
    	 ne 	 1]
    	 
    	 (let [se (first rlst)]
    	 
    	 (if (= fe se)
    	   (recur se
    	   	  (rest rlst)
    	   	  (inc ne))
    	   (if (= ne 1)
    	     (conj (f rlst) fe)
    	     (conj (f rlst) (vector ne fe)))))))
    
(defn encode-modified
  "Only elements with duplicates are converted to [n e] vectors."
  [lst]
  (if (empty? lst)
    lst
    (encode-one-letter-modified lst encode-modified)))
    
(defn decode-one-letter
  [n e]
  (if (zero? n)
    ()
    (conj (decode-one-letter (dec n) e) e)))
    
(defn decode
  "Returns the decoded version of lst."
  [lst]    
  (if (empty? lst)
    ()
    (let [v (first lst)]
      (if (vector? v)
      	 (let [n (first v)
               e (first (rest v))]
    	 (flatten (conj (decode (rest lst)) (decode-one-letter n e))))
         (conj (decode (rest lst)) v)))))
    
  
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))
    
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))
    
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))
    
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))       
        
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))
    	 
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))
    
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))
      
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))      
       
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))
    
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
         (exchange true 42 '((true) 42 ((cool (42)) (true))))))
    
(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four) 
         (my-flatten '(((one) ((two))) () (three (())) four)))))
      
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))
    
(deftest test-enlist
  (is (= () (enlist '())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))
    
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March]) 
         (invert-pairs '([January 1][February 2][March 3])))))
  
(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

(run-tests)
