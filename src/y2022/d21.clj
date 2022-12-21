(ns y2022.d21
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def test-data "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

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

(defn build-tree
  "Given a starting symbol, construct the expression given by a map of symbols->expressions"
  [from-symbol expressions]
  (let [exp (get expressions from-symbol)]
    (if (or (number? exp) (nil? exp)) (or exp from-symbol)
        (let [[op & rest] exp]
          (apply list op (map #(build-tree % expressions) rest))))))
(comment
  (build-tree 'root (parse test-data))
  ;; (+ (/ (+ 4 (* 2 (- 5 3))) 4) (* (- 32 2) 5))
  )

(defn simplify
  "Simplify an expression down to its simplest form by evaluating
   any expressions consisting entirely of numbers."
  [expression]
  (walk/postwalk (fn [x]
                   (if (and (list? x) (every? number? (rest x)))
                     (apply (resolve (first x)) (rest x))
                     x)) expression))
(comment
  (simplify (build-tree 'root (parse test-data)))
  ;; 152
  (simplify (build-tree 'root (dissoc (parse test-data) 'humn)))
  ;; (+ (/ (+ 4 (* 2 (- humn 3))) 4) 150)
  )

(defn test-number
  "Given a test-number, modify the expression-map so that the value
   for 'humn' is replaced by the test number, and the operator for
   'root' is replaced with the compare function. This allows us to
   estimate which direction to go when testing the next number."
  [test-number expressions]
  (let [expressions (-> expressions
                        (assoc 'humn test-number)
                        (update 'root (fn [[_ & args]]
                                        (apply list 'compare args))))]
    (simplify (build-tree 'root expressions))))

(defn part-1 [data]
  (simplify (build-tree 'root (parse data))))

(defn part-2
  "Given a high upper and lower number, test each extreme value
   and the value in between to narrow down the search area.
   Repeat until we find the correct number for the expression."
  [data]
  (let [p (parse data)]
    (loop [n 1000 ;; Loop failsafe
           upper-bound 10000000000000
           lower-bound -10000000000000]
      (let [test-num (long (/ (+ upper-bound lower-bound) 2))
            result-upper (test-number upper-bound p)
            result-lower (test-number lower-bound p)
            result-mid (test-number test-num p)]
        (cond (or (zero? n) (zero? result-mid)) test-num
              (= result-mid result-lower) (recur (dec n) upper-bound test-num)
              (= result-mid result-upper) (recur (dec n) test-num lower-bound))))))

(assert (= 152 (part-1 test-data)))
(assert (= 301 (part-2 test-data)))

(comment
  (part-1 (slurp "input/2022/21.txt"))
  (part-2 (slurp "input/2022/21.txt")))