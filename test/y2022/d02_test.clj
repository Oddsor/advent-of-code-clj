(ns y2022.d02-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d02 :refer [part-1 part-2]]))

(def test-data "A Y
B X
C Z")

(deftest part-1-test
  (is (= 15 (part-1 test-data))))
(deftest part-2-test
  (is (= 12 (part-2 test-data))))