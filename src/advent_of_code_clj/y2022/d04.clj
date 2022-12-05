(ns advent-of-code-clj.y2022.d04 
  (:require [clojure.set :as set]))

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

(comment 
  (part-1 (slurp "input/2022/04.txt"))
  (part-2 (slurp "input/2022/04.txt")))