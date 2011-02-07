(ns exercises.laziness
  (:use [clojure.java.io :only (reader as-url)]))

(defn url-get [url]
  (with-open [in (reader (as-url url))]
    (doall (line-seq in))))

; ex1
(defn counting-spider [urls]
  (doall (for [url urls]
           (do (println (str "Fetching " url))
               (count (url-get url))))))

; ex2
(defn print-squares [n]
  (doseq [i (range n)]
    (println (* i i))))

; ex3
(def smallest-with-factors-72-108
  (first (filter #(and (= (mod % 72) 0)
                       (= (mod % 108) 0)
                       (not= % 0))
                 (range))))

; ex4
(defn super-composite [n]
  (let [divisible-by-1-to-n? (fn [i]
                               (and (not= i 0)
                                    (every? #(= (mod i %) 0)
                                            (range 1 (inc n)))))]
    (first (filter divisible-by-1-to-n? (range)))))

; ex5
(defn indexes [seq]
  (map #(identity %2) seq (range)))

; ex6
(defn inits [seq]
  (concat (map #(take % seq) (indexes seq))
          [seq]))

; ex7
(defn sum-halve [seq]
  (let [halves (map #(split-at %1 seq)
                    (rest (indexes seq)))
        same-sum (filter (fn [[x y]]
                           (= (apply + x)
                              (apply + y)))
                         halves)]
    (first same-sum)))

; ex8
(defn nonempty-tails [seq]
  (take-while (complement empty?)
              (iterate rest seq)))

; ex9
(defn subseqs [seq]
  (mapcat nonempty-tails (rest (inits seq))))

; ex10
(defn subseq-sum [sum seq]
  (first (filter #(= (apply + %) sum)
                 (subseqs seq))))
