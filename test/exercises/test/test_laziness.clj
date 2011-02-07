(ns exercises.test.test-laziness
  (:use exercises.laziness
        midje.sweet)
  (:import [java.io StringWriter]))

(defmacro capturing-output [f]
  `(let [output# (StringWriter.)
         ret# (binding [*out* output#]
                ~f)]
     [ret# (str output#)]))

(facts "counting-spider"
       (let [base "http://cs.helsinki.fi/u/ivacklin/"
             url-for-n (fn [n] (str base "clojure-" n ".txt"))
             urls (map url-for-n [0 1 2])]
         (capturing-output
           (counting-spider urls))
             => [[0 1 2]
                 (apply str (map #(str "Fetching " % "\n") urls))]))

(facts "print-squares"
       (capturing-output
         (print-squares 4))
           => [nil (apply str (map #(str % "\n") [0 1 4 9]))])

(facts "super-composite"
       (super-composite 3)  => 6
       (super-composite 4)  => 12
       (super-composite 5)  => 60
       (super-composite 6)  => 60
       (super-composite 7)  => 420
       (super-composite 8)  => 840
       (super-composite 12) => 27720)

(facts "indexes"
       (indexes []) => empty?
       (indexes [:cow :cow :cow]) => [0 1 2]
       (take 10 (indexes (range))) => (range 10))

(facts "inits"
       (inits '(1 2 3 4))       => (just empty? [1] [1 2] [1 2 3] [1 2 3 4])
       (take 5 (inits (range))) => (just empty? [0] [0 1] [0 1 2] [0 1 2 3]))

(facts "sum-halve"
       (sum-halve [1 1 1 1])
         => (just [1 1] [1 1])
       (sum-halve [1 2 3 6])
         => (just [1 2 3] [6])
       (sum-halve (concat (range 10) (range 10)))
         => (just [0 1 2 3 4 5 6 7 8 9] [0 1 2 3 4 5 6 7 8 9])
       (sum-halve [1 1 2 4 8 16 30 2])
         => (just [1 1 2 4 8 16] [30 2]))

(facts "nonempty-tails"
       (nonempty-tails []) => empty?
       (nonempty-tails (seq [1 2 3 4]))
         => (just [1 2 3 4] [2 3 4] [3 4] [4])
       (take 10 (nth (nonempty-tails (range)) 5))
         => (just [5 6 7 8 9 10 11 12 13 14]))

(facts "subseqs"
       (subseqs [1 2]) => (just [[1 2] [1] [2]] :in-any-order)
       (subseqs [1 2 3 4]) => (just [[1 2 3 4] [1 2 3] [1 2] [1] [2 3 4] [2 3] [2] [3 4] [3] [4]] :in-any-order))

(defn pred-or [& rst]
  (fn [val] (reduce #(or %1 %2) (map #(% val) rst))))

(facts "subseq-sum"
       (subseq-sum 0 [0 1 2 3 4 5])  => (just 0)
       (subseq-sum 9 [1 2 3 4 5])    => (just 2 3 4)
       (subseq-sum 12 [1 1 1])       => nil
       (subseq-sum 39 (range 100))
         => (pred-or (just [4 5 6 7 8 9])
                     (just [12 13 14])
                     (just [39])))

(facts "subseqs and subseq-sum should work on infinite sequences"
       (take 10 (subseqs (range)))
         => (just [[0] [0 1] [1] [0 1 2] [1 2] [2] [0 1 2 3] [1 2 3] [2 3] [3]]
                  :in-any-order)
       (subseq-sum 39 (range)) => (just [4 5 6 7 8 9]
                                        :in-any-order))
