(ns mutation)

(defn memo
  [fun]
  (let [cache (atom {})]
    (fn [x] 
      (if (contains? @cache x)
        (@cache x)
        (do 
          (fun x)
          (let [result (fun x)]
            (swap! cache assoc x result)
            result))))))

(def a1 (ref {:name "Joe" :total 100}))
(def a2 (ref {:name "Jill" :total 500}))

(defn transfer
  [x y value]
  (dosync
    (if (> value (@x :total))
      "Not enough founds!"
      (do
        (alter x assoc :total (- (@x :total) value))
        (alter y assoc :total (+ (@y :total) value))
        "Succes!"))))
