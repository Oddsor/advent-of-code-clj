(ns advent-of-code-clj.y2022.d06-test
  (:require [advent-of-code-clj.y2022.d06 :refer [part-1 part-1-dt part-2
                                                  part-2-dt]]
            [clojure.test :refer [deftest is]]))

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
  (is (= 7 (part-1-dt test-1)))
  (is (= 5 (part-1-dt test-2)))
  (is (= 6 (part-1-dt test-3)))
  (is (= 10 (part-1-dt test-4)))
  (is (= 11 (part-1-dt test-5))))

(deftest part-2-rolling-test
  (is (= 19 (part-2-dt test-1)))
  (is (= 23 (part-2-dt test-2)))
  (is (= 23 (part-2-dt test-3)))
  (is (= 29 (part-2-dt test-4)))
  (is (= 26 (part-2-dt test-5))))
