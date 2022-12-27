(ns y2022.d01
  (:require [advent-of-code-clj.utils :refer [emap split-newline]]
            [clojure.string :as str]))

(defn- sum [xs] (reduce + xs))
(def ^:private sort-desc (partial sort >))

(defn- parse [text]
  (->> text split-newline (map str/split-lines) (emap parse-long)))

(defn sorted-calories [data]
  (->> data (map sum) sort-desc))

;; Part 1 - find the maximum calories carried by one elf
;; Could use "(apply max ...)" instead of sorting, but part 2
;; needs the maximum 3 calorie counts, so a sorted list is more useful
(defn part-1 [data]
  (->> data parse sorted-calories first))

;; Part 2 - find the amount of calories carried by the "top 3" elves
(defn part-2 [data]
  (->> data parse sorted-calories (take 3) sum))

(comment
  (= 66186 (part-1 (slurp "input/2022/01.txt")))
  (= 196804 (part-2 (slurp "input/2022/01.txt"))))