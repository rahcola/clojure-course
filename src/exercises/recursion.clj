(ns exercises.recursion
  (:use [exercises.collections :only (snip)]))

; ex1
(defn rsum [seq]
  (if (empty? seq)
	0
	(+ (first seq) (rsum (rest seq)))))

(defn rsum2 [seq]
  (cond (empty? seq) 0
		(= (count seq) 1) (first seq)
		:else (let [tail (rest seq)
					sum (+ (first seq) (first tail))]
				(recur (cons sum (rest tail))))))

; ex1
(defn product [s]
  (if (empty? s)
    1
    (* (first s) (product (rest s)))))

; ex2
; (product [1 2 3])
; (* 1 (product [2 3]))
; (* 1 (* 2 (product [3])))
; (* 1 (* 2 (* 3 (product []))))
; (* 1 (* 2 (* 3 1)))
; (* 1 (* 2 3)
; (* 1 6)
; 6

; ex3
(defn last-elem [seq]
  (if (empty? (rest seq))
    (first seq)
    (last-elem (rest seq))))

; ex4
(defn sequence-contains? [seq a]
  (cond (empty? seq)
        false
        (= (first seq) a)
        true
        :else
        (recur (rest seq) a)))

; ex5
(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2))
        true
        (or (empty? seq1) (empty? seq2))
        false
        (not= (first seq1) (first seq2))
        false
        :else
        (recur (rest seq1) (rest seq2))))

; ex6
(defn power [n p]
  (if (zero? p)
    1
    (* n (power n (dec p)))))

; ex7
(defn fib [n]
  (cond (<= n 0)
        0
        (= 1 n)
        1
        :else
        (+ (fib (- n 1)) (fib (- n 2)))))

; ex8
(defn my-range [n]
  (if (pos? n)
    (cons (dec n) (my-range (dec n)))))

; ex9
(defn map1 [fn seq]
  (if (empty? seq)
    seq
    (cons (fn (first seq))
          (map1 fn (rest seq)))))

; ex10 FIXME
(defn snip-many [seq]
  (if (empty? seq)
    (list seq)
    (let [[head tail] (snip seq)]
      (cons head (snip-many tail)))))

; ex11
(defn tails [seq]
  (if (empty? seq)
    (list seq)
    (cons seq (tails (rest seq)))))

(defn inits [seq]
  )

()

; ex3
(defn fibonacci-n [n]
  (if (<= n 2)
	1
	(+ (fibonacci-n (- n 1))
	   (fibonacci-n (- n 2)))))

(defn fibonacci-n2 [n]
  (take n ((fn fib []
			 (lazy-cat [1] [1] (map + (fib) (rest (fib))))))))

; ex4
(defn rotations [s]
  (let [rotate-once (fn  [s]
					  (if (empty? s)
						s
						(concat (rest s) (list (first s)))))
		rotates (fn  [acc]
				  (let [r (rotate-once (first acc))]
					(if (or (empty? r) (= r s))
					  acc
					  (recur (cons r acc)))))]
	(let [s (seq [(seq s)])]
	  (rotates s))))

(defn permutations [s]
  (let [perms (fn [[fst & rest]]
				(map (fn [e] (cons fst e)) (permutations rest)))]
	(if (empty? s)
	  (list s)
	  (apply concat (map perms (rotations s))))))

(defn powerset [s]
  (if (empty? s)
	(list (list))
	(let [augment-subsets (fn [e T] (map #(cons e %) T))
		  e (first s)
		  t (rest s)]
	  (concat (augment-subsets e (powerset t))
			  (powerset t)))))
