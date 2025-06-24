(ns y2024.d12-test
  (:require
    [clojure.test :refer [deftest is]]
    [y2024.d12 :as sut]))

(deftest part-1-test
  (is (= 1930
         (sut/total-price sut/circumference-price sut/test-input))))

(deftest part-2-test
  (is (= 1206
         (sut/total-price sut/sides-price sut/test-input)))
  (is (= 368
         (sut/total-price sut/sides-price sut/test-input-part-2))
      "Testcase where part of the region meets diagonally"))
