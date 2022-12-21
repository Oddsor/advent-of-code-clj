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
                                     {:op (symbol x)
                                      :rhs [(symbol ls) (symbol rs)]}))})))]
    (into {} expressions)))

(defn split-resolved-syms [expressions]
  [(into {} (filter (fn [[_ v]]
                      (number? v)) expressions))
   (into {} (remove (fn [[_ v]]
                      (number? v)) expressions))])

(defn replace-known-symbols [known-syms expressions]
  (into {} (map (fn [[lhs {:keys [op rhs] :as exp}]]
                  {lhs
                   (let [rhs (map (fn [x] (known-syms x x)) rhs)]
                     (if (every? number? rhs)
                       (apply (resolve op) rhs)
                       (assoc exp
                              :rhs rhs)))}) expressions)))

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
    ((last (resolv expressions)) 'root)))
(assert (= 152 (part-1 test-data)))

(comment
  (part-1 (slurp "input/2022/21.txt")))