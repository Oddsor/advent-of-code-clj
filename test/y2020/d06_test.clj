(ns y2020.d06-test
  (:require [clojure.test :refer [deftest is]]
            [advent-of-code-clj.y2020.d06 :refer [part-1 part-2]]))

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