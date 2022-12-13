(ns y2022.d02-test
  (:require [clojure.test :refer [deftest is]]
            [advent-of-code-clj.y2022.d02 :refer :all]))

(def test-data "A Y
B X
C Z")

(deftest part-1-test
  (is (= 15 (part-1 test-data))))
(deftest part-2-test
  (is (= 12 (part-2 test-data))))