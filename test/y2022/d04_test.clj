(ns y2022.d04-test
  (:require [advent-of-code-clj.y2022.d04 :refer [part-1 part-2]]
            [clojure.test :refer [deftest is]]))

(def test-data "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(deftest part-1-test
  (is (= 2 (part-1 test-data))))
(deftest part-2-test
  (is (= 4 (part-2 test-data))))