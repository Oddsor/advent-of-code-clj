(ns y2022.d14-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d14 :refer [part-1 part-2]]))

(def test-data "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(deftest part-1-test
  (is (= 24 (part-1 test-data))))
(deftest part-2-test
  (is (= 93 (part-2 test-data))))