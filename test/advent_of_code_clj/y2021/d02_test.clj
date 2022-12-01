(ns advent-of-code-clj.y2021.d02-test
  (:require [advent-of-code-clj.y2021.d02 :refer :all]
            [clojure.test :refer [deftest is]]))

(def test-data "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(deftest part-1
  (is (= {:depth 10 :h-pos 15}
         (navigate-1 (parse test-data))))
  (is (= 150 (multiply-positions
              (navigate-1 (parse test-data))))))
(deftest part-2
  (is (= {:depth 60, :h-pos 15, :aim 10}
         (navigate-2 (parse test-data))))
  (is (= 900 (multiply-positions
              (navigate-2 (parse test-data))))))