(ns y2024.d01-test
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.test :refer [deftest is]]
    [y2024.d01 :as sut]))

(deftest part-1-test
  (is (= 11 (sut/total-difference sut/test-data)))
  (is (= 2756096 (sut/total-difference (input/get-input 2024 1)))))

(deftest part-2-test
  (is (= 31 (sut/total-similarity-score sut/test-data)))
  (is (= 23117829 (sut/total-similarity-score (input/get-input 2024 1)))))
