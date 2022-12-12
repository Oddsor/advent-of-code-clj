(ns y2022.d12-test 
  (:require [clojure.test :refer [deftest is]]
            [y2022.d12 :refer [part-1 part-2]]))

(def test-data "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(deftest part-1-test
  (is (= 31 (part-1 test-data)))) 

(deftest part-2-test
  (is (= 29 (part-2 test-data))))