(ns y2022.d21
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn parse [data]
  (let [expressions (->> data
                         str/split-lines
                         (map (partial re-seq #"[\+\-\*/\w]+"))
                         (map (fn [[left-side & right-side]]
                                {(symbol left-side)
                                 (if (= 1 (count right-side))
                                   (parse-long (first right-side))
                                   (let [[ls x rs] right-side]
                                     (map symbol (list x ls rs))))})))]
    (into {} expressions)))

(defn build-expression
  "Given a starting symbol, construct the expression given by a map of symbols->expressions"
  [from-symbol expressions]
  (let [exp (get expressions from-symbol)]
    (if (or (number? exp) (nil? exp)) (or exp from-symbol)
        (let [[op & rest] exp]
          (apply list op (map #(build-expression % expressions) rest))))))
(comment
  (build-expression 'root (parse test-data))
  ;; (+ (/ (+ 4 (* 2 (- 5 3))) 4) (* (- 32 2) 5))
  )

(defn simplify
  "Simplify an expression down to its simplest form by evaluating
   any expressions consisting entirely of numbers.
   
   Can also accept a map of known symbols. Used when testing a value
   in the expression."
  ([expression] (simplify {} expression))
  ([known-syms expression]
   (walk/postwalk (fn [x]
                    (cond
                      (and (list? x) (every? number? (rest x)))
                      (apply (resolve (first x)) (rest x))
                      (symbol? x) (known-syms x x)
                      :else x)) expression)))
(comment
  (simplify (build-expression 'root (parse test-data)))
  ;; 152
  (simplify (build-expression 'root (dissoc (parse test-data) 'humn)))
  ;; (+ (/ (+ 4 (* 2 (- humn 3))) 4) 150)
  (simplify {'humn 5} (build-expression 'root (dissoc (parse test-data) 'humn)))
  ;; 152
  )

(defn test-number
  "Given a test-number, modify the expression-map so that the value
   for 'humn' is replaced by the test number, and the operator for
   'root' is replaced with the compare function. This allows us to
   estimate which direction to go when testing the next number."
  [test-number expression]
  (simplify {'humn test-number} expression))

(defn part-1 [data]
  (simplify (build-expression 'root (parse data))))

(defn part-2
  "Given a high upper and lower number, test each extreme value
   and the value in between to narrow down the search area.
   Repeat until we find the correct number for the expression."
  [data]
  (let [exp (build-expression 'root (-> (parse data)
                                        (dissoc 'humn)
                                        (update 'root (fn [[_ & args]]
                                                        (apply list 'compare args)))))]
    (loop [upper-bound 10000000000000
           lower-bound -10000000000000]
      (let [test-num (long (/ (+ upper-bound lower-bound) 2))
            result-upper (test-number upper-bound exp)
            result-lower (test-number lower-bound exp)
            result-mid (test-number test-num exp)]
        (if (zero? result-mid) test-num
            (recur (if (= result-mid result-lower) upper-bound test-num)
                   (if (= result-mid result-upper) lower-bound test-num)))))))

(defn part-2-mathy
  "Since the expression is a tree, the expression is apparently linear given
   'humn', so we can perform math magic"
  [data]
  (let [exp (build-expression 'root (-> (parse data)
                                        (dissoc 'humn)
                                        (update 'root (fn [[_ & args]]
                                                        (apply list '- args)))))]
    (/ (simplify {'humn 0} exp)
       (- (simplify {'humn 0} exp) (simplify {'humn 1} exp)))))

(comment
  (= 83056452926300 (part-1 (slurp "input/2022/21.txt")))
  (= 3469704905529 (part-2 (slurp "input/2022/21.txt")))
  (= 3469704905529 (part-2-mathy (slurp "input/2022/21.txt"))))