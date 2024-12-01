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

;; This function splits a line, generates a regex-pattern from the right hand part (numbers), and a list of
;; combinations from the right hand part. Then we find the list of combinations that match the pattern:

(defn split-line
  ([line] (split-line 1 line))
  ([scale line]
   (let [[pattern numbers] (.split line " ")]
     [(map parse-long (re-seq #"\d+" (String/join "," (repeat scale numbers))))
      (.replaceAll
       (String/join "?" (repeat scale pattern))
       "\\.\\.+" ".")])))

(split-line (second (str/split-lines test-data)))

;; Mostly stolen from https://github.com/erdos/advent-of-code/blob/master/2023/day12.clj
;; Key lesson for me is to not try to collect all combinations and then filter them later
(defn valid-remainder [num s]
  (for [i (range (inc (- (count s) num)))
        :while (every? #{\. \?} (take i s))
        :when (every? #{\# \?} (take num (drop i s)))
        :when (not= \# (nth s (+ num i) \.))]
    (drop (+ (inc i) num) s)))

(assert (= [(list \? \. \# \# \#)
            (list \. \# \# \#)
            (list \# \# \#)]
           (valid-remainder 1 "???.###")))
(assert (= [[]]
           (valid-remainder 3 "###")))
(assert (= []
           (valid-remainder 3 "##")))
(assert (=
         [(list \? \? \? \? \? \? \?)]
         (valid-remainder 3 "?###????????")))

^{:nextjournal.clerk/visibility {:result :hide}}
(def find-arrangements
  (memoize
   (fn [[counts pattern]]
     (if-let [[c & rst] (seq counts)]
       (reduce +
               (for [remaining-chars (valid-remainder c pattern)]
                 (find-arrangements [rst remaining-chars])))
       (if (every? #{\. \?} pattern)
         1
         0)))))

(= 1 (find-arrangements (split-line "???.### 1,1,3")))
(= 4 (find-arrangements (split-line ".??..??...?##. 1,1,3")))
(= 1 (find-arrangements (split-line "?#?#?#?#?#?#?#? 1,3,1,6")))
(= 10 (find-arrangements (split-line "?###???????? 3,2,1")))

;; ## Part 1

;; Solution for the test set:

(= 21 (transduce (comp
                  (map split-line)
                  (map find-arrangements))
                 + (str/split-lines test-data)))

;; This gives us the following solution for the input (use pmap to parallelize and speed things up):

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (crit/quick-bench
   (= 21 (transduce (comp (map split-line)
                          (map find-arrangements))
                    + (str/split-lines test-data))))
  (crit/quick-bench
   (= 7771 (apply + (map (comp find-arrangements split-line)
                         (str/split-lines (slurp "input/2023/d12.txt")))))))

;; ## Part 2

;; Oops, we need to scale the problem 5x by repeating the patterns!

(comment
  (= 525152
     (transduce (comp (map #(split-line 5 %))
                      (map find-arrangements))
                + (str/split-lines test-data)))

  (= 10861030975833
     (transduce (comp (map #(split-line 5 %))
                      (map find-arrangements))
                + (str/split-lines (slurp "input/2023/d12.txt")))))
