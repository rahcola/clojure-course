(ns exercises.boss1)

(def books (load-file "src/exercises/books.clj"))

(defn author-has-years? [book]
  (let [m (book :author)]
	(and (contains? m :birth-year)
		 (contains? m :death-year))))

(defn books-with-author-years [books]
  (filter author-has-years? books))

(defn authors [books]
  (set (map #(% :author) books)))

(defn author-names [books]
  (map #(% :name) (authors books)))

(defn titles-by-author [author books]
  (map #(% :title) (filter #(= (% :author) author) books)))

(defn author-catalog [books]
  (if (empty? books)
    {}
    (apply
     conj {} (map #(vector % (titles-by-author % books))
                  (authors books)))))
