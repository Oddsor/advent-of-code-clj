(ns y2021.d01-test
  (:require [clojure.test :refer [deftest is]]
            [advent-of-code-clj.y2021.d01 :refer :all]))

(def test-data "199
200
208
210
200
207
240
269
260
263")

(deftest part-1
  (is (= 7 (part-1-recursive test-data)))
  (is (= 7 (measurements-higher-than-previous
            (parse test-data)))))

(deftest part-2
  (is (= 5 (-> test-data
               parse
               sliding-sum
               measurements-higher-than-previous))))