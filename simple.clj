;Comment

(ns simple
	(use clojure.test))


; defn -> define functions, you don't need to espicify the type of a function
(defn f2c
  "Converts x degrees Fahrenheit into Degrees Celsius"
  [x]
  (/ (* (- x 32) 5) 9))

(defn sign
  "It returns -1 if n is negative, 1 if n is positive greater than zero, or 0 if n is zero"
  [n]
  (if (< n 0)
    -1
     (if (> n 0)
     1
     0 )))
  
(defn roots
  "Returns a vector containing the two possible roots that solve a quadratic equation given its three coefficients (a, b, c)"
  [a b c]
  (let [d (- b)
  	e (Math/sqrt (- (* b b) (* 4 a c)))
  	f (* 2 a)
  	x1 (/ (+ d e) f)
  	x2 (/ (- d e) f)]
  	[x1 x2]))
  	
(defn bmi
	"return a symbol that represents the corresponding BMI description computed from its input."
	[w h]
	(let [bmi (/ w (* h h))]
	(if (< bmi 20) 
		'underweight
		(if (< bmi 25)
			'normal
			(if (< bmi 30)
				'obese1
				(if (< bmi 40)
					'obese2
					'obese3
	))))))
     
(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))
  
(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))
 
(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-0.25 -1] (roots 4 5 1))))

(deftest test-bmi
      (is (= 'underweight (bmi 45 1.7)))
      (is (= 'normal (bmi 55 1.5)))
      (is (= 'obese1 (bmi 76 1.7)))  
      (is (= 'obese2 (bmi 81 1.6)))
      (is (= 'obese3 (bmi 120 1.6))))
  
(run-tests)
