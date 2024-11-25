^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d12
  (:require [clojure.string :as str]))

;; # Year 2023; Day 12

;; Here we need to generate various combinations of a string, where '?' can represent either a dot or hash symbol.
;; The combinations need to match a particular sequence of hash-symbols separated by dots, given by the numbers
;; to the right in the data set. So for example, in the first line, the minimum matching string is "#.#.###"
;; (given the numbers 1, 1 and 3).

(def test-data "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

;; This function takes a string and generates combinations:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn combinations [xs]
  (loop [acc []
         [x & rx] xs]
    (if x
      (let [nx (if (= \? x) [\. \#] [x])]
        (recur (if (seq acc)
                 (mapcat (fn [x] (map #(str % x) acc)) nx)
                 nx) rx))
      acc)))

;; This function splits a line, generates a regex-pattern from the right hand part (numbers), and a list of
;; combinations from the right hand part. Then we find the list of combinations that match the pattern:

(defn split-line
  ([line] (split-line 1 line))
  ([scale line]
   (let [split (String/.split line " ")]
     [(String/join "?" (repeat scale (aget split 0)))
      (String/join "," (repeat scale (aget split 1)))])))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-arrangements [[xs counts]]
  (let [pattern (re-pattern (str "^\\.*" (String/join "\\.+" (map (fn [x]
                                                                    (str "#{" x "}")) (re-seq #"\d+" counts)))
                                 "\\.*$"))]
    (count (keep (partial re-matches pattern) (combinations xs)))))

(= 1 (find-arrangements (split-line "???.### 1,1,3")))
(= 4 (find-arrangements (split-line ".??..??...?##. 1,1,3")))
(= 1 (find-arrangements (split-line "?#?#?#?#?#?#?#? 1,3,1,6")))
(= 10 (find-arrangements (split-line "?###???????? 3,2,1")))

;; ## Part 1
;; Solution for the test set:

(= 21 (transduce (comp (map split-line)
                       (map find-arrangements))
                 + (str/split-lines test-data)))

;; This gives us the following solution for the input (use pmap to parallelize and speed things up):

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 7771 (apply + (pmap (comp find-arrangements split-line) (str/split-lines (slurp "input/2023/d12.txt"))))))

;; ## Part 2

;; Oops, we need to scale the problem 5x by repeating the patterns!

(mapv #(split-line 5 %) (str/split-lines test-data))

(comment
  (= 525152
     (transduce (comp (map #(split-line 2 %))
                      (map find-arrangements))
                + (str/split-lines test-data))))
