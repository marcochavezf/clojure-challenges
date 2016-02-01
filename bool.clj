(ns bool
  (:use 
    [clojure.pprint :only (cl-format)]
    [clojure.contrib.str-utils2 :only (repeat) :rename {repeat str-repeat}]))

(def operations {
  'not  not
  'and  #(and %1 %2)
  'or   #(or %1 %2)
  'xor  not=
  'nand #(not (and %1 %2))
  'nor  #(not (or %1 %2))
  'imp  #(not (and %1 (not %2)))
  'eqv  =
  })

(defn interpret
  [table expr]
  (cond 
    (#{0 1} expr) (= 1 expr)
    (symbol? expr) (table expr)
    (and (list? expr) (operations (first expr)))
      (apply 
        (operations (first expr))
        (map #(interpret table %) (rest expr)))
    :else (throw (Exception. "Invalid expression!"))))

(defn get-var-names
  [expr]
  (if (list? expr)
    (sort 
      (set
        (filter 
          #(not (operations %)) 
          (filter symbol? (flatten expr)))))
    (get-var-names (list expr))))

(defn make-rows
  [values n]
  (cond
    (zero? n) ()
    (= n 1) (map list values)
    :else
      (for [x values
      	    y (make-rows values (dec n))]
        (conj y x))))

(defn eval-all
  [expr]
  (let [var-names (get-var-names expr)
        n (count var-names)
        rows (make-rows [false true] n)]
    (for [row rows :let [table (zipmap var-names row)]]
      (concat row (list (interpret table expr))))))

(defn center
  [size item]
  (cl-format nil "~v:@<~A~>" size item))
  
(defn truth-table
  [expr]
  (let [var-names     (get-var-names expr)
  	headers       (concat var-names (list expr))
  	header-sizes (map #(count (str %)) headers)
  	char-count    (+ (reduce + header-sizes) (dec (count headers)))
  	dashed-line   (str-repeat \- char-count)]
    (println dashed-line)
    (apply println headers)
    (println dashed-line)
    (doseq [row (eval-all expr)]
      (apply
        println
        (map center header-sizes (map #(if % 1 0) row))))
    (println dashed-line)))
