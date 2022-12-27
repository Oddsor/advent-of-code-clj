(ns y2022.d06-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d06 :refer [*unique-sequence-end-index-fn* part-1 part-2
                               unique-sequence-end-index-2]]))

(def test-1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def test-2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def test-3 "nppdvjthqldpwncqszvftbrmjlhg")
(def test-4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def test-5 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(deftest part-1-test
  (is (= 7 (part-1 test-1)))
  (is (= 5 (part-1 test-2)))
  (is (= 6 (part-1 test-3)))
  (is (= 10 (part-1 test-4)))
  (is (= 11 (part-1 test-5))))

(deftest part-2-test
  (is (= 19 (part-2 test-1)))
  (is (= 23 (part-2 test-2)))
  (is (= 23 (part-2 test-3)))
  (is (= 29 (part-2 test-4)))
  (is (= 26 (part-2 test-5))))

(deftest part-1-rolling-test
  (with-bindings {#'*unique-sequence-end-index-fn* unique-sequence-end-index-2}
    (is (= 7 (part-1 test-1)))
    (is (= 5 (part-1 test-2)))
    (is (= 6 (part-1 test-3)))
    (is (= 10 (part-1 test-4)))
    (is (= 11 (part-1 test-5)))))

(deftest part-2-rolling-test
  (with-bindings {#'*unique-sequence-end-index-fn* unique-sequence-end-index-2}
    (is (= 19 (part-2 test-1)))
    (is (= 23 (part-2 test-2)))
    (is (= 23 (part-2 test-3)))
    (is (= 29 (part-2 test-4)))
    (is (= 26 (part-2 test-5)))))
