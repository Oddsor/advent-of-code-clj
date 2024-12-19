(ns advent-of-code-clj.utils-test
  (:require
   [aoc.test-utils :as tu]
   [advent-of-code-clj.utils :as utils]
   [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :once tu/with-instrumentation)

(deftest bisect-test
  (testing "Finds last valid value"
    (let [numbers (vec (range 0 1000000 19))
          found-index (utils/bisect #(> 100006 (numbers %)) 0 (dec (count numbers)))]
      (is (> (numbers (inc found-index)) 100006 (numbers found-index)))))
  (testing "Finds first valid value"
    (let [numbers (vec (range 0 1000000 19))
          found-index (utils/bisect #(< 100006 (numbers %)) 0 (dec (count numbers)))]
      (is (< (numbers (dec found-index)) 100006 (numbers found-index)))))
  (testing "Inverted greater-than/less-than does not find same value"
    (let [numbers (vec (range 0 1000000 19))
          found-index (utils/bisect #(< 100006 (numbers %)) 0 (dec (count numbers)))
          found-index-2 (utils/bisect #(> 100006 (numbers %)) 0 (dec (count numbers)))]
      (is (not= found-index found-index-2)))
    (let [numbers (vec (range 0 1000000))
          middle-value (/ 1000000 2)
          found-index (utils/bisect #(<= middle-value (numbers %)) 0 (dec (count numbers)))
          found-index-2 (utils/bisect #(> middle-value (numbers %)) 0 (dec (count numbers)))]
      (is (= 500000 found-index))
      (is (= 499999 found-index-2))
      (is (not= found-index found-index-2))))
  (testing "Throws when no value can be found"
    (let [numbers (vec (range 0 1000000 19))]
      (is (thrown? Exception (utils/bisect #(= 199 (numbers %)) 0 (dec (count numbers))))))
    (let [numbers (vec (repeat 1000 0))]
      (is (thrown? Exception (utils/bisect #(= 0 (numbers %)) 0 (dec (count numbers))))))
    (let [numbers (vec (concat (range 0 400) (range 1000 0) (range 0 400)))]
      (is (thrown? Exception (utils/bisect #(> (numbers %) 500) 0 (dec (count numbers))))))))
