(ns exercises.test.test-calculatrix
  (:use midje.sweet
        exercises.calculatrix)
  (:import (java.util.concurrent
             TimeoutException TimeUnit FutureTask)))

(facts "read-words"
       (with-in-str "foo bar" (read-words)
         => ["foo" "bar"])
       (with-in-str "4 2   1" (read-words)
         => ["4" "2" "1"]))

(facts "string->number"
       (string->number "2")   => 2
       (string->number "0")   => 0
       (string->number "2 3") => nil
       (string->number "foo") => nil)

(facts "compute"
       (compute "+" ["2" "2"])   => 4
       (compute "*" ["3" "2"])   => 6
       (compute "*" ["bar" "2"]) => nil
       (compute "foo" ["2" "2"]) => nil)

(defn test-main [input]
  (with-open [output (java.io.StringWriter.)]
    (let [f (fn []
              (binding [*out* output]
                (with-in-str (str input "\n") (main))))
          get-output (fn []
                       (.trim 
                         (second (.split (str output)
                                         "=>"))))
          task (FutureTask. f)
          thr (Thread. task)]
      (try
        (.start thr)
        (.get task 10 TimeUnit/MILLISECONDS)
        (get-output)
        (catch TimeoutException e
          (get-output))))))

(facts "main"
       (test-main "+ 2 2") => "4"
       (test-main "* 0 1") => "0")
