(ns htmlexample
  (use html))

(println
(webdoc
  (html
    (head
      (meta* {:char-set "utf-8"})
      (title "Example"))
    (body
      (h1 "Example")
      (img
        {:src "blabla.jpg"
        :alt "bblablaimage"})
      (p "This is an example "
        (strong "of a program ")
        "written in CLojure")))))
