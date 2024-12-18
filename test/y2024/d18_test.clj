(ns y2024.d18-test
  (:require
   [advent-of-code-clj.input :as input]
   [clojure.test :refer [deftest is]]
   [y2024.d18 :as sut]))

(deftest part-1-test
  (is (= 22 (sut/part-1 [6 6] 12 sut/test-input)))
  (is (= 264 (sut/part-1 [70 70] 1024 (input/get-input 2024 18)))))

(deftest part-2-test
  (is (= "6,1" (sut/part-2 [6 6] sut/test-input)))
  (is (= "41,26" (sut/part-2 [70 70] (input/get-input 2024 18)))))
