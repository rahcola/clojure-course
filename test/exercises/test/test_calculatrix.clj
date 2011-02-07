(ns exercises.test.test-calculatrix
  (:use midje.sweet
        exercises.calculatrix)
  (:import (java.util.concurrent
             TimeoutException TimeUnit FutureTask)))

(def *timeout* 200)

(defn test-main [input]
  (with-open [output (java.io.StringWriter.)]
    (let [f (fn []
              (binding [*out* output]
                (with-in-str (str input "\n" "\n")
                  (main))))
          get-output (fn []
                       (.trim
                         (last (.split (str output)
                                       "=>"))))
          task (FutureTask. f)
          thr (Thread. task)]
      (try
        (.start thr)
        (.get task *timeout* TimeUnit/MILLISECONDS)
        (get-output)
        (catch TimeoutException e
          (get-output))))))

(facts "read-words"
       (with-in-str "foo bar" (read-words))
         => ["foo" "bar"]
       (with-in-str "4 2   1" (read-words))
         => ["4" "2" "1"]
       (with-in-str "\n" (read-words))
         => [""])

(facts "string->number"
       (string->number "2")   => 2
       (string->number "0")   => 0
       (string->number "2 3") => nil
       (string->number "foo") => nil)

(facts "Charmeleon"
       (test-main "- 32 3") => "29")

(facts "Miles Edgeworth"
       (test-main "+ 2 foo")
         => "Invalid operand: foo"
       (test-main "foo 2 3")
         => "Invalid command: foo")

(facts "Manfred von Karma"
       (test-main "+ 1 1\n* _ 2") => "4")

(facts "The Milkman Conspiracy"
       (test-main "avg 1 2") => "3/2"
       (test-main "pow 2 3") => "8")

(facts "Godot"
       (test-main "store a 2\n* a 3") => "6"
       (test-main "store a 42\nstore b a\n+ a b") => "84")

(facts "Bowser"
       (test-main "pow 0 2 3")
         => "Wrong number of arguments to pow: expects between 2 and 2, you gave 3.")

(facts "main"
       (test-main "+ 2 2") => "4"
       (test-main "* 0 1") => "0")
