(ns y2022.d24-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d24 :refer [part-1 part-2]]))

(def test-data "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(deftest part-1-test
  (is (= 18 (part-1 test-data))))

(deftest part-2-test
  (is (= 54 (part-2 test-data))))