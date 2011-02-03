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

(def fn-table
  {
   "+" (fn [& xs] (reduce + 0 xs))
   "-" (fn [x & xs]
         (if (empty? xs)
           (- x)
           (reduce - x xs)))
   "*" (fn [& xs] (reduce * 1 xs))
   "avg" (fn [x y] (/ (+ x y) 2))
   "pow" (fn [x e] (int (Math/pow x e)))
   })

(defn lookup [var-table var]
  (get var-table var (str "Variable " var " not defined")))

(defn apply-operator [operator args]
  (if (not-every? number? args)
    (apply str
           "Invalid operands: "
           (map #(str % " ")
                (filter #(not (number? %)) args)))
    (try
      (apply operator args)
      (catch IllegalArgumentException e
        (.getMessage e)))))

(defn compute [command args fn-table var-table]
  (let [store (fn [var-table var value]
                (let [value (first (compute value [] {} var-table))]
                  (if (number? value)
                    [(str "Stored " var "=" value)
                     (assoc var-table var value)]
                    [(str "Invalid value: " value)
                     var-table])))]
    (cond (number? (string->number command))
          [(string->number command) var-table]
          (and (empty? args) (nil? (fn-table command)))
          [(lookup var-table command) var-table]
          (= "store" command)
          (apply store var-table args)
          :else
          (let [operator (fn-table command)
                args (map #(first (compute % [] fn-table var-table))
                          args)]
            (if (nil? operator)
              [(str "Invalid command: " command) var-table]
              [(apply-operator operator args) var-table])))))

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
