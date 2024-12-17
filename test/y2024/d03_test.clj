(ns y2024.d03-test
  (:require
   [advent-of-code-clj.input :as input]
   [clojure.test :refer [deftest is]]
   [y2024.d03 :as sut]))

(deftest part-1-test
  (is (= 161 (sut/parse-and-multiply "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")))
  (is (= 173419328 (sut/parse-and-multiply (input/get-input 2024 3)))))

(deftest part-2-test
  (is (= 48 (sut/parse-and-multiply-when "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")))
  (is (= 48 (sut/process-operations (sut/get-operations "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))))
  (is (= 90669332 (sut/process-operations (sut/get-operations (input/get-input 2024 3))))))

(deftest part-2-again-test
  (is (= 90669332 (sut/parse-and-multiply (sut/prep-input (input/get-input 2024 3)))))) 
