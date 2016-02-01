(ns arithmetic
	(:use clojure.test)
	(:use clojure.contrib.trace))
	
(defn add1 [x] (conj x ()))

(def sub1 rest)

(def equal-zero? empty?)

(def zero ())
(def one (add1 zero))
(def two (add1 one))
(def three (add1 two))
(def four (add1 three))
(def five (add1 four))
(def six (add1 five))


(defn add            
;(deftrace add
	"Return a plus b"
	[a b]
	(if (equal-zero? a)
	  b
	  (add1 (add (sub1 a) b))))
	  
(defn subtract
  "Return a minus b. Assume that a >= b."
  [a b]
  (if (equal-zero? b)
    a
    (sub1 (subtract a (sub1 b)))))
    
(defn multiply
  "Returns a times b."
  [a b]
  (if (equal-zero? a)
    zero
    (add b (multiply (sub1 a) b))))

(defn equal?
  "Returns true if a is equal to b, otherwise returns false."
  [a b]
  (if (equal-zero? a)
    (if (equal-zero? b)
      true
      false)
    (if (equal-zero? b)
      false
      (equal? (sub1 a) (sub1 b)))))

(defn less?
  "Returns true if a is less than b, otherwise returns false."
  [a b]
  (if (equal-zero? a)
    (if (equal-zero? b)
      false
      true)
    (if (equal-zero? b)
      false
      (less? (sub1 a) (sub1 b)))))
      
(defn qoutient
  "Returns the quotient of dividing a by b. Assumes b > 0"
  [a b]
  (if (less? a b)
    zero
    (add1 (qoutient (subtract a b) b))))
    
(defn remainder
  "Returns the remainder of dividing a by b."
  [a b]
  (if (less? a b)
    a
    (remainder (subtract a b) b)))

(defn power
  "Returns a raised to the power of b."
  [a b]
  (if (equal-zero? b)
    one
    (multiply a (power a (sub1 b)))))

(deftest test-add
  (is (= three (add zero three)))
  (is (= five (add two three)))
  (is (= five (add two three)))
  (is (= five (add five zero))))
  
(deftest test-subtract
  (is (= five (subtract five zero)))
  (is (= two (subtract four two)))
  (is (= four (subtract five one)))
  (is (= zero (subtract three three))))
  
(deftest test-multiply
  (is (= zero (multiply zero five)))
  (is (= six (multiply two three)))
  (is (= zero (multiply five zero)))
  (is (= four (multiply one four))))
 
(deftest test-equal?
  (is (equal? five five))
  (is (not (equal? four three)))
  (is (not (equal? three five))) 
  (is (equal? zero zero)))
  
(deftest test-less?
  (is (not (less? five five)))
  (is (not (less? zero zero)))
  (is (not (less? four three)))
  (is (less? three five))
  (is (less? three five)))
  
(deftest test-qoutient
  (is (= zero (qoutient three four)))
  (is (= six (qoutient six one)))
  (is (= two (qoutient five two)))
  (is (= zero (qoutient zero four))))
  
(deftest test-remainder
  (is (= three (remainder three four)))
  (is (= zero (remainder six one)))
  (is (= one (remainder five two)))
  (is (= zero (remainder zero four))))
  
(deftest test-power
  (is (= one (power five zero)))
  (is (= five (power five one)))
  (is (= four (power two two)))
  (is (= one (power one six))))
  
(run-tests)
