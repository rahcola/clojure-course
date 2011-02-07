(ns exercises.calculatrix)

(defn read-words []
  "Read a line and split it into words. Returns the words as a vector
  of strings."
  (let [line (read-line)]
    (vec (.split line "\\s+"))))

(defn string->number [string]
  (try
    (Integer/parseInt string)
    (catch NumberFormatException e nil)))

(defprotocol ICalcFn
  (calc-call [this args]))

(defn rigth-number-of-args? [min arg-count max]
  (and (<= min arg-count)
       (or (= max :inf)
           (<= arg-count max))))

(defn wrong-n-args [name arg-count min max]
  (str "Wrong number of arguments to " name ":"
       " expects between " min " and " max
       ", you gave " arg-count "."))

(deftype CalcFn [name fn arity-min arity-max]
  ICalcFn
  (calc-call [this args]
             (let [arg-count (count args)]
               (if (rigth-number-of-args? arity-min arg-count arity-max)
                 (apply fn args)
                 (wrong-n-args name
                               arg-count
                               arity-min
                               arity-max)))))

(deftype StoreFn [fn arity-min arity-max]
  ICalcFn
  (calc-call [this args]
             (let [arg-count (count args)]
               (if (rigth-number-of-args? arity-min arg-count arity-max)
                 (apply fn args)
                 [(wrong-n-args "store"
                                arg-count
                                arity-min
                                arity-max)
                  (first args)]))))

(defn lookup [var-table var]
  (get var-table var (str "Variable " var " not defined")))

(defn apply-operator [operator args]
  (if (not-every? number? args)
    (apply str
           "Invalid operands: "
           (map #(str % " ")
                (filter #(not (number? %)) args)))
    (calc-call operator args)))

(defn compute [command args fn-table var-table]
  (cond (number? (string->number command))
        [(string->number command) var-table]
        (and (empty? args) (nil? (fn-table command)))
        [(lookup var-table command) var-table]
        (= "store" command)
        (calc-call (fn-table :store) (cons var-table args))
        :else
        (let [operator (fn-table command)
              args (map #(first (compute % [] fn-table var-table))
                        args)]
          (if (nil? operator)
            [(str "Invalid command: " command) var-table]
            [(apply-operator operator args) var-table]))))

(def fn-table
  {
   "+" (CalcFn. "+"
                (fn [& xs] (reduce + 0 xs))
                0
                :inf)
   "-" (CalcFn. "-"
                (fn [x & xs]
                  (if (empty? xs)
                    (- x)
                    (reduce - x xs)))
                1
                :inf)
   "*" (CalcFn. "*"
                (fn [& xs] (reduce * 1 xs))
                0
                :inf)
   "avg" (CalcFn. "avg"
                  (fn [x y] (/ (+ x y) 2))
                  2
                  2)
   "pow" (CalcFn. "pow"
                  (fn [x e] (int (Math/pow x e)))
                  2
                  2)
   :store (StoreFn. (fn [var-table var value]
                      (let [value (first (compute value [] {} var-table))]
                        (if (number? value)
                          [(str "Stored " var "=" value)
                           (assoc var-table var value)]
                          [(str "Invalid value: " value)
                           var-table])))
                    3
                    3)
   })

(defn main-fn [last-result var-table]
  "This is the driver loop of the calculator. It loops by calling
   itself recursively."
  (let [words (read-words)
        command (first words)]
    (when (not (empty? command))
      (let [arguments (replace {"_" (str last-result)}
                               (rest words))
            [result var-table] (compute command
                                        arguments
                                        fn-table
                                        var-table)]
        (println "  =>" result)
        (recur result var-table)))))

(defn main []
  (main-fn nil {}))
