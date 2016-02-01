(ns queens
  (:use [clojure.contrib.generic.math-functions :only (abs)]))

(defn attack?
  [pos1 pos2]
  (or
    (= (pos1 0) (pos2 0))
    (= (pos1 1) (pos2 1))
    (= (abs (- (pos1 0) (pos2 0))) (abs (- (pos1 1) (pos2 1)))))) 

(defn safe? 
  [board row]
  (not
    (some
      #(attack? [row (board row)] [% (board %)])
      (range row))))

(defn find-boards
  [board row]
  (let [n           (count board)
  	next-row    (inc row)
  	safe-boards (filter
  		      #(safe? % row)
		        (map
			  #(assoc board row %)
			  (range n)))]
       (if (= n next-row)
         safe-boards
         (apply concat ((if (zero? row) pmap map) #(find-boards % next-row) safe-boards)))))

(defn solve
  [n]
  (find-boards (vec (repeat n -1)) 0))
