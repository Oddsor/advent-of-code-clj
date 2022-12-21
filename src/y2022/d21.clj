(ns y2022.d21
  (:require [clojure.string :as str]))

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

(defn split-resolved-syms [expressions]
  [(into {} (filter (fn [[_ v]]
                      (number? v)) expressions))
   (into {} (remove (fn [[_ v]]
                      (number? v)) expressions))])

(defn replace-known-symbols [known-syms expressions]
  (into {} (map (fn [[lhs [op & rhs]]]
                  {lhs
                   (let [rhs (map (fn [x] (known-syms x x)) rhs)]
                     (if (every? number? rhs)
                       (apply (resolve op) rhs)
                       (apply list op rhs)))}) expressions)))

(defn resolv
  ([expressions]
   (resolv {} expressions))
  ([syms exps]
   (lazy-seq
    (cons exps (let [[s e] (split-resolved-syms exps)]
                 (if (empty? e) nil
                     (resolv s (replace-known-symbols (merge syms s) e))))))))

(defn part-1 [data]
  (let [expressions (parse data)]
    (-> (resolv expressions) last (get 'root))))

(assert (= 152 (part-1 test-data)))

(comment
  (part-1 (slurp "input/2022/21.txt")))

(defn test-number [expressions number]
  (let [new-expressions (-> expressions
                            (update 'root (fn [[_ & rest]] (apply list 'compare rest)))
                            (assoc 'humn number))]
    (-> new-expressions resolv last (get 'root) (= 0))))

(defn part-2 [data]
  (let [expressions (parse data)]
    (first (filter (partial test-number expressions) (range -10000 100000)))))

(assert (= 301 (part-2 test-data)))
(comment (part-2 (slurp "input/2022/21.txt")))