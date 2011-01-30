(ns exercises.basics
  (:use [clojure.contrib.string :only (split)]))

; ex1
(defn square [x] (* x x))

; ex2
(defn first-half [s]
  (subs s 0 (/ (count s) 2)))

; ex3
(defn rectangle-area [w h]
  (* w h))

; ex4
(defn avg [a b]
  (/ (+ a b) 2))

; ex5
(defn abs [n]
  (if (< n 0)
    (- n)
    n))

; ex6
(defn fizzbuzz [n]
  (if (= (mod n 15) 0)
	(println "gotcha!")
	(if (= (mod n 3) 0)
	  (println "fizz")
	  (if (= (mod n 5) 0)
		(println "buzz")))))

; ex8
(defn teen? [age]
  (<= 13 age 19))

; ex9
(defn not-teen? [age]
  (not (teen? age)))

; ex10
(defn spiff [v]
  (+ (nth v 0) (nth v 2)))

; ex11
(defn cutefy [coll]
  (conj coll "<3"))

; ex12
(defn spiff2 [v]
  (let [fst (first v)
		rd (nth v 2)]
	(+ fst rd)))

(def cities {:author "China Mieville" :title "The City and the City"})

; ex13
(defn title-length [book]
  (let [title (:title book)]
	(count title)))

(def books-pages {{:author "China Mieville" :title "The City and the City"} 500
				  {:author "Haruki Murakami" :title "Norwegian Wood"} 400
				  {:author "Guy Gavriel Kay" :title "Under Heaven"} 576})

; ex14
(defn add-number-of-pages [books book]
  (conj book [:number-of-pages (get books book)]))

; ex15
(defn first-elems [vs]
  (for [v vs]
	(first v)))

; ex16
(defn sort-by-keys [m]
  (for [key (sort (keys m))]
	  (get m key)))

(def books [{:author "China Miéville" :title "Kraken"}
            {:author "China Miéville" :title "The City and the City"}
            {:author "Haruki Murakami" :title "Norwegian Wood"}
            {:author "Guy Gavriel Kay" :title "Under Heaven"}])

; ex17
(defn books-by-author [author books]
  (for [book books :when (= (book :author) author)] book))

; ex18
(defn books-by-author2 [author books]
  (filter #(= (% :author) author) books))

; ex19
(defn snd-of-colls [colls]
  (map #(nth % 1) colls))

; ex20
(defn str-to-list [s]
  (map #(Integer. (str  %)) (filter #(not (= \, %)) s)))

(defn str-to-list2 [s]
  (map  #(Integer. (.trim %1))
		(filter #(not (empty? %1)) (split #"(,| +)" s))))

; ex21
(defn consecutives [coll]
  (map vector coll (rest coll)))
