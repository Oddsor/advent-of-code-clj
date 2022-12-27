(ns y2022.d21-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d21 :refer [part-1 part-2 part-2-mathy]]))

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

(deftest part-1-test
  (is (= 152 (part-1 test-data))))
(deftest part-2-test
  (is (= 301 (part-2 test-data)))
  (is (= 301 (part-2-mathy test-data))))