(ns exercises.predicates
  (:use [exercises.boss1 :only (books)]))

; ex 1
(defn generic-doublificate [a]
  (cond (number? a)
        (* 2 a)
        (and (coll? a) (empty? a))
        nil
        (or (list? a) (vector? a))
        (map #(generic-doublificate %) a)
        :else true))

; ex 2
(defn empty-string? [s]
  (let [whitespace? (fn [c] (Character/isWhitespace c))]
    (every? whitespace? s)))

; ex3
(defn pred-and [p1 p2]
  (fn [x] (and (p1 x) (p2 x))))

; ex4
(defn every-book-has-a-title? [books]
  (every? #(contains? % :title) books))

; ex5
(defn prime? [n]
  (let [p (fn [x] (= (mod n x) 0))]
    (not (some p (range 2 n)))))

; ex6
(defn first-value-for-key [k maps]
  (get (some #(if (contains? % k) %) maps) k)
  (some k maps))

