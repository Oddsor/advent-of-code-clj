(ns y2020.d06-test
  (:require [advent-of-code-clj.y2020.d06 :refer [part-1 part-2]]
            [clojure.test :refer [deftest is]]))

(def test-data "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest part-1-test
  (is (= 11 (part-1 test-data))))

(deftest part-2-test
  (is (= 6 (part-2 test-data))))