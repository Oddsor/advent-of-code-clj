(ns advent-of-code-clj.y2022.d01
  (:require [advent-of-code-clj.utils :refer [partition-parse]]))

(defn- sum [xs] (reduce + xs))
(def ^:private sort-desc (partial sort >))

(defn- parse-data [text]
  (partition-parse parse-long text))

(defn sorted-calories [data]
  (->> data (map sum) sort-desc))

;; Part 1 - find the maximum calories carried by one elf
;; Could use "(apply max ...)" instead of sorting, but part 2
;; needs the maximum 3 calorie counts, so a sorted list is more useful
(defn part-1 [data]
  (->> data parse-data sorted-calories first))

;; Part 2 - find the amount of calories carried by the "top 3" elves
(defn part-2 [data]
  (->> data parse-data sorted-calories (take 3) sum))

(comment
  (part-1 (slurp "input/2022/01.txt"))
  (part-2 (slurp "input/2022/01.txt")))