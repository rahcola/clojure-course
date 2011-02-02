(ns exercises.test.test-collections
  (:use midje.sweet
        exercises.collections))

(fact "doublificate"
      (doublificate {"a" 1 "b" 7}) => {"a" 1, "double-a" 2
                                       "b" 7 "double-b" 14})

(facts "halve"
       (halve [1 2 3 4])   => (just (just 1 2) (just 3 4))
       (halve [1 2 3 4 5]) => (just (just 1 2 3) (just 4 5)))

(facts "first-five-positives"
       (first-five-positives [1 3 -2 3 2 0 10 -1 42]) => (just 1 3 3 2 10)
       (first-five-positives [1 -2 9 -4 -5])          => (just 1 9))

(fact "snip"
      (snip [1 4 2 :snip 8 2 9]) => (just (just 1 4 2) (just 8 2 9)))

(facts "monotonic?"
       (monotonic? [1 2 3])   => truthy
       (monotonic? [3 2 1 0]) => truthy
       (monotonic? [3 2 2])   => truthy
       (monotonic? [1 2 1 0]) => falsey)

(facts "transpose"
       (transpose [[1 2 3]
                   [4 5 6]
                   [7 8 9]
                   [0 0 0]])
           => [[1 4 7 0]
               [2 5 8 0]
               [3 6 9 0]]
       (transpose []) => []
       (transpose [[1 2 3]]) => [[1] [2] [3]])

(facts "exterminate"
       (exterminate {})                         => {}
       (exterminate {3 2, 5 1})                 => {}
       (exterminate {1 3, 4 2, 5 7, 10 9, 8 8}) => {1 3, 5 7, 8 8})

(facts "take-3"
       (take-3 [1 2 3 4 5]) => [1 2 3]
       (take-3 ["Haruki" "Murakami"]) => ["Haruki" "Murakami" nil])

(facts "author-to-string"
       (author-to-string {:name "Shakespeare, William"
                          :birth-year 1564, :death-year 1616})
           => "Shakespeare, William (1564-1616)"
       (author-to-string {:name "Doyle, Arthur Conan, Sir"
                          :birth-year 1859, :death-year 1930})
           => "Doyle, Arthur Conan, Sir (1859-1930)")


(fact "book-to-string"
      (book-to-string {:title "Nuoren Robertin matka Grönlantiin isäänsä hakemaan"
                       :author {:name "Hoffmann, Franz"
                                :birth-year 1814, :death-year 1882}})
          => "A book, Nuoren Robertin matka Grönlantiin isäänsä hakemaan, written by Hoffmann, Franz (1814-1882)")

(facts "my-keys"
       (my-keys {:name "Doyle, Arthur Conan, Sir"
                 :birth-year 1859, :death-year 1930})
           => (just :name :birth-year :death-year)
       (my-keys {:title "Norwegian Wood" :author "Haruki Murakami"})
           => (just :title :author))

(fact "who-wrote"
      (who-wrote [{:author "Me" :title "Best Book"}
                  {:author "Sam" :title "This Other Book"}] 1)
          => "Sam")

(def books [{:author {:name "Haruki Murakami"
                      :birth-year 1949}
             :title "Norwegian Wood"}
            {:author {:name "David Mitchell"}
             :title "The Thousand Autumns of Jacob de Zoet"}
            {:author {:name "Guy Gavriel Kay"
                      :birth-year 1954}
             :title "Under Heaven"}])

(fact "author-birth-years"
      (author-birth-years books) => (just 1949 1954))

(fact "add-at"
      (add-at [[1 2 3]
               [4 5 6]
               [7 8 9]]
              [1 1]
              [2 1])    => [[1 2  3]
                            [4 13 6]
                            [7 8  9]])
