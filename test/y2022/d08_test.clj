(ns y2022.d08-test
  (:require [advent-of-code-clj.y2022.d08 :refer [part-1 part-2]]
            [clojure.test :refer [deftest is]]))

(def test-data "30373
25512
65332
33549
35390")

(deftest part-1-test
  (is (= 21 (part-1 test-data))))
(deftest part-2-test
  (is (= 8 (part-2 test-data))))