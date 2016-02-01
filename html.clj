;;;; Building web pages with Clojure.
;;;; An example using macros.

(ns html
  (:use [clojure.contrib.string :only (as-str chop join)]))

(def CRLF "\r\n")

(def escape-codes {
  \& "&amp;"
  \< "&lt;"
  \> "&gt;"
  \" "&#34;"
  \' "&#39;"
  })

(defn escape-html
  [s]
  (join "" (map #(get escape-codes % %) s)))

(defn str-attrs
  [attrs]
  (join ""
    (map
      (fn [[key value]] (format " %s=\"%s\"" (as-str key) (escape-html value)))
      attrs)))

(def to-string)

(defn start-end-tag
  [[name attrs content]]
  (str "<" name (str-attrs attrs)
    (if content
      (str ">" (join "" (map to-string content)) "</" name ">")
      "/>")))

(defn to-string
  [tree]
  (cond
    (nil? tree) ""
    (string? tree) (escape-html tree)
    (= 'safe (first tree)) (second tree)
    :else (start-end-tag tree)))

(defn webdoc
  [tree]
  (str "<!DOCTYPE html>" CRLF (to-string tree)))

(defn remove-star
  [name]
  (let [str-name (str name)]
    (symbol
      (if (= \* (last str-name))
        (chop str-name)
        str-name))))

(defmacro defelement
  [name]
  (let [real-name (remove-star name)]
    `(defn ~name
      [& lst#]
      (let [[head# & tail#] lst#]
        (if (map? head#)
          (list (quote ~real-name) head# tail#)
          (list (quote ~real-name) {} lst#))))))

(defn safe
  [s]
  (assert (string? s))
  (list 'safe s))

(defelement a)
(defelement body)
(defelement br)
(defelement div)
(defelement em)
(defelement form)
(defelement h1)
(defelement h2)
(defelement h3)
(defelement h4)
(defelement h5)
(defelement h6)
(defelement head)
(defelement hr)
(defelement html)
(defelement img)
(defelement input)
(defelement label)
(defelement li)
(defelement link)
(defelement meta*)
(defelement p)
(defelement span)
(defelement pre)
(defelement strong)
(defelement title)
(defelement ul)

