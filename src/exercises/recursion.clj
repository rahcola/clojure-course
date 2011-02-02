(ns exercises.recursion
  (:use [exercises.collections :only (snip halve)]))

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
(defn last-element [seq]
  (if (empty? (rest seq))
    (first seq)
    (last-element (rest seq))))

; ex4
(defn sequence-contains? [a seq]
  (cond (empty? seq)
        false
        (= (first seq) a)
        true
        :else
        (recur a (rest seq))))

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
(defn map-1 [fn seq]
  (if (empty? seq)
    seq
    (cons (fn (first seq))
          (map-1 fn (rest seq)))))

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
  (if (empty? seq)
    (cons '() '())
    (cons seq (inits (butlast seq)))))

(defn inits [seq]
  (if (empty? seq)
    (cons '() '())
    (map reverse (tails (reverse seq)))))

; ex12
(defn split-into-monotonics [seq]
  (if (empty? seq)
    '()
    (let [monotonic? (fn [seq] (or (apply <= seq)
                                   (apply >= seq)))
          split-help (fn [pre suf]
                       (if (monotonic? pre)
                         (list pre suf)
                         (recur (butlast pre)
                                (cons (last pre)
                                      suf))))
          [pre suf] (split-help seq '())]
      (cons pre (split-into-monotonics suf)))))

; ex13
(defn rotations [seq]
  (rest (map concat
             (reverse (tails seq))
             (inits seq))))

; ex14
(defn my-frequencies [seq]
  (let [fre-help (fn [seq acc]
                   (if (empty? seq)
                     acc
                     (recur (rest seq)
                            (assoc acc
                              (first seq)
                              (inc (get acc
                                        (first seq)
                                        0))))))]
    (fre-help seq {})))

; ex15
(defn un-frequencies [map]
  (if (empty? map)
    '()
    (let [[what times] (first map)]
      (concat (repeat times what) (un-frequencies (rest map))))))

; ex16
(defn seq-merge [seq1 seq2]
  (let [merge-help (fn [seq1 seq2 acc]
                     (cond (empty? seq1)
                           (concat acc seq2)
                           (empty? seq2)
                           (concat acc seq1)
                           :else
                           (let [s1 (first seq1)
                                 s2 (first seq2)]
                             (if (< s1 s2)
                               (recur (rest seq1) seq2 (concat acc [s1]))
                               (recur seq1 (rest seq2) (concat acc [s2]))))))]
    (merge-help seq1 seq2 '())))

; ex17
(defn mergesort [seq]
  (if (<= (count seq) 1)
    seq
    (let [[fst-half snd-half] (halve seq)]
      (seq-merge (mergesort fst-half)
                 (mergesort snd-half)))))

; ex18
(defn permutations [s]
  (let [perms (fn [[fst & rest]]
				(map (fn [e] (cons fst e)) (permutations rest)))]
	(cond (empty? s)
          '()
          (= 1 (count s))
          (list s)
          :else
          (apply concat (map perms (rotations s))))))

; ex19
(defn powerset [s]
  (if (empty? s)
	(list (list))
	(let [augment-subsets (fn [e T] (map #(cons e %) T))
		  e (first s)
		  t (rest s)]
	  (concat (augment-subsets e (powerset t))
			  (powerset t)))))
