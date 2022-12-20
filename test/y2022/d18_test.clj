(ns y2022.d18-test
  (:require [y2022.d18 :refer [part-1]]))

(def test-data "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(assert (= 64 (part-1 test-data)))