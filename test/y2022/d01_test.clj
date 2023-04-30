(ns y2022.d01-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d01 :refer [part-1 part-2]]))

(def test-data "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(deftest most-calories-carried-by-one-elf
  (is (= 24000 (part-1 test-data))))

(deftest most-calories-carried-by-3-elves
  (is (= 45000 (part-2 test-data))))