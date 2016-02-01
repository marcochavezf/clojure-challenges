;;; ITESM CEM, November 29, 2011.
;;; Clojure Source File
;;; Activity: Macros
;;; Author: Marco Chávez

(ns macros)

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & lst]
    `(let [v# ~x]
    	(if v# v# (my-or ~@lst)))))

(defmacro randomly
  [fst & rst]
  `(let [v# (cons '~fst '~rst)] (eval (nth v# (int (rand (count v#)))))))

(defmacro infix
  ([lst]
  `(if (not (list? '~lst))
    '~lst
    (let [x#  (first '~lst)
    	  op# (first (rest '~lst))
    	  y#  (last '~lst)]
    	  `(list op#
    	  	 (infix x#) 
    	  	 (infix y#))
    	  ))))

(defmacro do-loop
  [fst & rst]
  `(let [lstf#  (last '~rst)
  	 kw#    (first lstf#)
  	 cnd#  (last lstf#)]
    (if (= :while kw#)
      (loop []
        (if cnd#
          (do
            (do-loop-aux ~fst (rest lstf#))
            (recur))))
      (if (= :until kw#)
        (loop []
        (if (not cnd#)
          (do
            (do-loop-aux ~fst (rest lstf#))
            (recur))))))))	  

(defmacro do-loop-aux
  ([x] x)
  ([fst & rst]
  `(do
    ~fst
    (do-loop-aux  ~@rst))))
    
    

