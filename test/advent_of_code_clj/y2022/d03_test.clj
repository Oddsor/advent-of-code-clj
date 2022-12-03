(ns advent-of-code-clj.y2022.d03-test 
  (:require [clojure.test :refer [deftest is]]
            [advent-of-code-clj.y2022.d03 :refer :all]))

(def test-data "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(deftest part-1-test
  (is (= 157 (part-1 test-data)))) 

(deftest part-2-test
  (is (= 70 (part-2 test-data))))