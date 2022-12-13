(ns y2022.d13-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d13 :refer [in-order? part-1 part-2]]))

(def test-data "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(deftest in-order-test
  (is (neg-int? (in-order? [1,1,3,1,1] [1,1,5,1,1])))
  (is (neg-int? (in-order? [[1],[2,3,4]] [[1],4])))
  (is (pos-int? (in-order? [9] [[8,7,6]])))
  (is (neg-int? (in-order? [[4,4],4,4] [[4,4],4,4,4])))
  (is (neg-int? (in-order? [[4,4],4,4] [[4,4 4]])))
  (is (pos-int? (in-order? [[4,4 4],4,4] [[4,4],4,4,4]))))

(deftest part-1-test
  (is (= 13 (part-1 test-data))))

(deftest part-2-test
  (is (= 140 (part-2 test-data))))