(ns y2024.d11-test
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.test :refer [deftest is]]
    [y2024.d11 :as sut]))

(deftest part-1-test
  (is (= 55312 (sut/solve 25 sut/test-input)))
  (is (= 194557 (sut/solve 25 (input/get-input 2024 11)))))

(deftest part-2-test
  (is (= 65601038650482 (sut/solve 75 sut/test-input)))
  (is (= 231532558973909 (sut/solve 75 (input/get-input 2024 11)))))
