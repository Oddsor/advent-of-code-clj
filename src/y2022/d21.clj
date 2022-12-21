(ns y2022.d21
  (:require [clojure.string :as str]
            [numeric.expresso.core :refer [solve ex]]))

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

(defn expresso-parse [data]
  (let [expressions (->> data
                         str/split-lines
                         (map (partial re-seq #"[\+\-\*/\w]+"))
                         (map (fn [[left-side & right-side]]
                                (list 'ex (list '=
                                                (symbol left-side)
                                                (if (= 1 (count right-side))
                                                  (parse-long (first right-side))
                                                  (let [[ls x rs] right-side]
                                                    (list (symbol x) (symbol ls) (symbol rs)))))))))]
    (apply list 'solve (quote (quote root)) expressions)))

(defn part-1 [data]
  (val (ffirst (eval (expresso-parse data)))))
(assert (= 152 (part-1 test-data)))

(comment
  (part-1 (slurp "input/2022/21.txt")))