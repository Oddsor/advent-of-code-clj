(ns y2024.d02-test
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.test :refer [deftest is]]
    [y2024.d02 :as sut]))

(deftest part-1-test
  (is (= 2 (sut/safe-lines sut/test-data)))
  (is (= 314 (sut/safe-lines (sut/to-matrix (input/get-input 2024 2))))))

(deftest part-2-test
  (is (= 4 (sut/safeish-lines sut/test-data)))
  (is (= 373 (sut/safeish-lines (sut/to-matrix (input/get-input 2024 2))))))
