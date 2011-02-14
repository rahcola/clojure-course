(ns exercises.functional)

; ex1
(defn concat-all [ss]
  (reduce concat ss))

; ex2
(defn string-cat [strings]
  (reduce #(str %1 " " %2) strings))

; ex3
(defn seq-length [seq]
  (reduce (fn [len _] (inc len)) 0 seq))

; ex4
(defn insertion-sort [numbers]
  (let [insert
        (fn [sorted n]
          (loop [before []
                 after sorted]
            (let [next (first after)]
              (if (or (empty? after)
                      (< n next))
                (concat (conj before n) after)
                (recur (conj before next)
                       (rest after))))))]
    (reduce insert [] numbers)))

; ex5
(defn seq-min-max [seq]
  (let [min-max
        (fn [[minim maxim] candidate]
          [(min minim candidate) (max maxim candidate)])
        fst (first seq)]
    (reduce min-max [fst fst] (rest seq))))

; ex6

; ex7
(def reciprocal (partial / 1))

; ex8
