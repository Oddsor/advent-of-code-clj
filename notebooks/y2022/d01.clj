^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2022.d01
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/open-graph
   {:title "Y2022 - Day 1"}}
  (:require [advent-of-code-clj.utils :refer [emap sort- split-newline sum]]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [advent-of-code-clj.input :as input]))


; # Advent of code 2022, part 01

; Create a parser to convert the following test-data into lists:

^{:nextjournal.clerk/visibility {:code :hide}} (def test-data "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

{:nextjournal.clerk/visibility {:code :show :result :hide}}

(defn- parse [text]
  (->> text split-newline (map str/split-lines) (emap parse-long)))

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/example
 (parse test-data))

{:nextjournal.clerk/visibility {:code :show :result :show}}

;; ## Part 1
;; > find the maximum calories carried by one elf

;; Could use "(apply max ...)" instead of sorting, but part 2
;; needs the maximum 3 calorie counts, so a sorted list is more useful

^{:nextjournal.clerk/visibility {:result :hide}}
(defn sorted-calories [data]
  (->> data (map sum) sort-))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (->> data parse sorted-calories first))
(part-1 test-data)

;; This yields the expected result, so we apply it to our input:

(= 66186 (part-1 (input/get-input 2022 1)))

;; ## Part 2
;; > find the amount of calories carried by the "top 3" elves

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (->> data parse sorted-calories (take 3) sum))

(part-2 test-data)

;; Test output looks ok, so let's apply it to the read input

(= 196804 (part-2 (input/get-input 2022 1)))
