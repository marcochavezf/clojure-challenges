(ns higherorder
  (:use clojure.test))

(defn my-map-indexed
  "Returns a list consisting of the result of applying f to 0 and the first item of lst, followed by applying f to 1 and the second item in lst, and so on until lst is exhausted. Function f should accept 2 arguments: index and item."
  [f lst]
  (loop [lst lst
         i   0
         coll ()]
         (if (empty? lst)
           (reverse coll)
           (recur (rest lst) (inc i) (cons (f i (first lst)) coll)))))

(defn my-drop-while
  [f lst]
  (if (empty? lst)
     lst
     (if (f (first lst))
       (my-drop-while f (rest lst))
       lst)))
  
(defn bisection
  "Finds the corresponding root using the bisection method."
  [a b f]
  (let [c (/ (+ a b) 2)]
  (cond
    (and (< (f a) 0) (> (f c) 0)) (bisection a c f)
    (and (< (f c) 0) (> (f b) 0)) (bisection c b f)
    (< (Math/abs (f c)) (Math/pow 10 (- 15))) c 
    :else c)))
    
(defn deriv
  [f h]
  (fn [x] (/ (- (f (+ x h)) (f x)) h)))
  
(defn integral
  [a b n f]
  (let  [h (/ (- b a) n)]
  (loop [y 0
         r 0]
         (if (= y 0)
           (recur (inc y) (+ r (f (+ a (* y h)))))
           (if (= y n)
             (* (/ h 3) (+ r (f (+ a (* y h)))))
             (if (even? y)
               (recur (inc y) (* 4 (+ r (f (+ a (* y h))))))
               (recur (inc y) (* 2 (+ r (f (+ a (* y h))))))))))))
       
(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))
         
(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))   
           
;(deftest test-bisection
;  (is (= 3 (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
;  (is (= -4 (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
;  (is (= Math/PI (bisection 1 4 (fn [x] (Math/sin x)))))
;  (is (= (* 2 Math/PI) (bisection 5 10 (fn [x] (Math/sin x)))))
;  (is (= 1.618033988749895
;         (bisection 1 2 (fn [x] (- (* x x) x 1)))))
;  (is (= -0.6180339887498948
;         (bisection -10 1 (fn [x] (- (* x x) x 1))))))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (= 75 (Math/round (df 5))))
  (is (= 30 (Math/round (ddf 5))))
  (is (= 6 (Math/round (dddf 5)))))
  
(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
           (fn [x]
             (integral 3 4 10
               (fn [y]
                 (* x y))))))))
      
(run-tests)
