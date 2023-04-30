^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2022.d01
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/open-graph
   {:title "Y2022 - Day 1"}}
  (:require [advent-of-code-clj.utils :refer [emap sort- split-newline sum]]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide}}
(clerk/md "# Advent of code 2022, part 01")

; Create a parser to convert the following test-data into lists:

^{::clerk/visibility {:code :hide}} (def test-data "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

{::clerk/visibility {:code :show :result :hide}}

(defn- parse [text]
  (->> text split-newline (map str/split-lines) (emap parse-long)))

(defn sorted-calories [data]
  (->> data (map sum) sort-))

{::clerk/visibility {:code :show :result :show}}

;; ## Part 1
;; find the maximum calories carried by one elf
;; Could use "(apply max ...)" instead of sorting, but part 2
;; needs the maximum 3 calorie counts, so a sorted list is more useful

^{::clerk/visibility {:result :hide}}
(defn part-1 [data]
  (->> data parse sorted-calories first))
(part-1 test-data)

;; ## Part 2
;; find the amount of calories carried by the "top 3" elves

^{::clerk/visibility {:result :hide}}
(defn part-2 [data]
  (->> data parse sorted-calories (take 3) sum))
(part-2 test-data)

;; Test output looks ok, so let's apply it to the read input

^{::clerk/visibility {:code :hide}}
(clerk/example
 (= 66186 (part-1 (slurp "input/2022/01.txt")))
(= 196804 (part-2 (slurp "input/2022/01.txt"))))