(ns y2022.d20-test 
  (:require [clojure.test :refer [deftest is]]
            [y2022.d20 :refer [part-1 part-2]]))

(def test-data "1
2
-3
3
-2
0
4")

(deftest part-1-test
  (is (= 3 (part-1 test-data)))) 
(deftest part-2-test
  (is (= 1623178306 (part-2 test-data)))) 