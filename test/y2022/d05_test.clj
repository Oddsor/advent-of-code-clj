(ns y2022.d05-test
  (:require [advent-of-code-clj.y2022.d05 :refer [part-1 part-2]]
            [clojure.test :refer [deftest is]]))

(def test-data "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(deftest part-1-test
  (is (= "CMZ" (part-1 test-data))))
(deftest part-2-test
  (is (= "MCD" (part-2 test-data))))