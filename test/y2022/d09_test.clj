(ns y2022.d09-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d09 :refer [part-1 part-2]]))

(def test-data "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def larger-test-data "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(deftest part-1-test
  (is (= 13 (part-1 test-data))))

(deftest part-2-test
  (is (= 1 (part-2 test-data)))
  (is (= 36 (part-2 larger-test-data))))