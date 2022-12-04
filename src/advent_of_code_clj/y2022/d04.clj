(ns advent-of-code-clj.y2022.d04 
  (:require [clojure.set :as set]))

(def test-data "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn overlaps [[a b c d]]
  (or (<= a c d b)
      (<= c a b d)))

(defn overlaps-at-all [[a b c d]]
  (seq (set/intersection
        (set (range a (inc b))) 
        (set (range c (inc d))))))

(defn part-1 [data]
  (->> data
       (re-seq #"\d+")
       (map parse-long)
       (partition 4)
       (filter overlaps)
       count))

(defn part-2 [data]
  (->> data
       (re-seq #"\d+")
       (map parse-long)
       (partition 4)
       (filter overlaps-at-all)
       count))

(assert (= 2 (part-1 test-data)))
(assert (= 4 (part-2 test-data)))
(comment 
  (part-1 (slurp "input/2022/04.txt"))
  (part-2 (slurp "input/2022/04.txt")))