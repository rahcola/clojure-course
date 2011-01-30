(ns exercises.test.test-boss1
  (:use midje.sweet
        exercises.boss1))

(def test-books [{:title "Nuoren Robertin matka Grönlantiin isäänsä hakemaan", :author {:name "Hoffmann, Franz", :birth-year 1814, :death-year 1882}} {:title "Kirkkotie", :author {:name "Lewetzow, Cornelia"}} {:title "Naapurimme; Margreta\nKaksi kertomusta", :author {:name "Lewetzow, Cornelia"}} {:title "Purimossa\nViisinäytöksinen näytelmä", :author {:name "Halme, Kaarle", :birth-year 1864, :death-year 1946}}])

(def book-with-years {:title "Nuoren Robertin matka Grönlantiin isäänsä hakemaan"
                       :author {:name "Hoffmann, Franz"
                                :birth-year 1814
                                :death-year 1882}})

(def hoffman (:author book-with-years))

(def book-without-years {:title "Kirkkotie", :author {:name "Lewetzow, Cornelia"}})

(def book-1 {:title "Nuoren Robertin matka Grönlantiin isäänsä hakemaan"
             :author {:name "Hoffmann, Franz"
                      :birth-year 1814
                      :death-year 1882}})
(def book-2 {:title "Ihmiskohtaloja"
             :author {:name "Järnefelt, Arvid"
                      :birth-year 1861
                      :death-year 1932}})
(def book-3 {:title "Elämän meri"
             :author {:name "Järnefelt, Arvid"
                      :birth-year 1861
                      :death-year 1932}})

(def jarnefelt {:name "Järnefelt, Arvid"
                 :birth-year 1861
                 :death-year 1932})

(facts "author-has-years"
       (author-has-years? book-with-years)    => true
       (author-has-years? book-without-years) => false)

(facts "books-with-author-years"
       (books-with-author-years []) => empty?
       (books-with-author-years [book-with-years]) => (just book-with-years)
       (books-with-author-years [book-without-years]) => empty?)

(facts "authors"
       (authors []) => empty?
       (authors [book-with-years]) => (just hoffman)
       (authors [book-with-years book-with-years]) => (just hoffman))

(facts "author-names"
       (author-names []) => empty?
       (author-names [book-with-years]) => (just "Hoffmann, Franz")
       (author-names [book-with-years book-with-years]) => (just "Hoffmann, Franz"))

(facts "titles-by-author"
       (titles-by-author "" []) => empty?
       (titles-by-author hoffman
                         [book-with-years]) => (just "Nuoren Robertin matka Grönlantiin isäänsä hakemaan")
       (titles-by-author jarnefelt
                         [book-2 book-3]) => (just (:title book-2)
                                                   (:title book-3)))

(facts "author-catalog"
       (author-catalog []) => empty?
       (author-catalog [book-1]) => (just {hoffman (just (:title book-1))})
       (author-catalog [book-1 book-2 book-3])
       => (just {hoffman (just (:title book-1))
                 jarnefelt (just (:title book-2) (:title book-3))}))
