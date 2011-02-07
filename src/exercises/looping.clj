(ns exercises.looping
  (:use [exercises.calculatrix :only (read-words string->number)]))

; ex1
(defn power [n k]
  (let [pow (fn [acc k]
              (cond (= k 0)
                    1
                    (= k 1)
                    acc
                    :else
                    (recur (* acc n)
                           (dec k))))]
    (pow n k)))

; ex2
(defn last-element [seq]
  (if (<= (count seq) 1)
    (first seq)
    (recur (rest seq))))

; ex3
(defn seq= [seq1 seq2]
  (cond (and (empty? seq1)
             (empty? seq2))
        true
        (or (empty? seq1)
            (empty? seq2)
            (not= (first seq1)
                  (first seq2)))
        false
        :else
        (recur (rest seq1)
               (rest seq2))))

; ex4
(defn find-first-index [pred seq]
  (cond (empty? seq)
        nil
        (pred (first seq))
        (first seq)
        :else
        (recur pred (rest seq))))

; ex5
(defn avg [numbers]
  (if (empty? numbers)
    nil
    (loop [numbers numbers
           sum 0
           how-many 0]
      (if (empty? numbers)
        (/ sum how-many)
        (recur (rest numbers)
               (+ sum (first numbers))
               (inc how-many))))))

; ex6
(defn ask-avg []
  (let [input (read-words)]
    (when (not (empty? (first input)))
      (println "avg:" (avg (map string->number input)))
      (recur))))

; ex7
(defn parity [seq]
  (loop [seq seq
         set #{}]
    (cond (empty? seq)
          set
          (contains? set (first seq))
          (recur (rest seq)
                 (disj set (first seq)))
          :else
          (recur (rest seq)
                 (conj set (first seq))))))

; ex8
(defn fast-fibo [n]
  (loop [n n
         fib-2 0
         fib-1 1]
    (if (< n 1)
      fib-2
      (recur (dec n)
             fib-1
             (+ fib-2 fib-1)))))

; ex9
(defn cut-at-repetition [seq]
  (loop [seq seq
         result []]
    (cond (or (empty? seq))
          result
          (= (first seq) (second seq))
          (conj result (first seq))
          :else
          (recur (rest seq)
                 (conj result (first seq))))))
