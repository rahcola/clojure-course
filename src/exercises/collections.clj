(ns exercises.collections)

; ex1
(defn doublificate [m]
  (into m (map #(vector (str "double-" (nth % 0))
                        (* 2 (nth % 1)))
               (seq m))))

; ex2
(defn halve [s]
  (let [splitp (/ (count s) 2)]
    (seq (vector (take splitp s)
                 (drop splitp s)))))

; ex3
(defn first-five-positives [s]
  (take 5 (filter pos? s)))

; ex4
(defn snip [s]
  [(take-while #(not= :snip %) s)
   (rest (drop-while #(not= :snip %) s))])

; ex5
(defn monotonic-prefix [s]
  (if (or (apply <= s) (apply >= s))
    s
    (recur (butlast s))))

; ex6
(defn describe-books [books]
  (apply str "I have " (count books) " books."
         (map #(str " "
                    (% :title)
                    " was written by "
                    (% :author)
                    ".")
              books)))

; ex7
(defn monotonic? [s]
  (if (empty? s)
    true
    (or (apply <= s) (apply >= s))))

; ex8
(defn transpose [s]
  (if (empty? s)
    '()
    (apply (partial map vector) s)))

; ex9
(defn exterminate [m]
  (into {} (filter (fn [[x y]] (<= x y)) m)))

; ex10
(defn take-3 [[fst snd rd & rest]]
  (list fst snd rd))

; ex11
(defn my-keys [m]
  (for [[key val] m] key))

; ex12
(defn author-to-string [author]
  (let [{name :name
         birth :birth-year
         death :death-year} author]
    (str name " (" birth "-" death ")")))

; ex13
(defn book-to-string [book]
  (let [{title :title
         author :author} book]
    (str "A book, " title
         ", written by " (author-to-string author))))

; ex14
(defn who-wrote [books i]
  ((books i) :author))

; ex15
(defn author-birth-years [books]
  (filter #(not (nil? %))
          (map #(get-in % [:author :birth-year])
               books)))

; ex16
(defn add-at [matrix a b]
  (update-in matrix a #(+ % (get-in matrix b))))