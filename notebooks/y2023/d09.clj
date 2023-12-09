(ns y2023.d09
  (:require [clojure.string :as str]))

(def test-data "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

;; Create a helper function to extract all numbers (possibly negative) from a piece of text

(defn get-numbers [line] (mapv parse-long (re-seq #"-?\d+" line)))

;; Create a function to find the difference between each number in a sequence.
;; Creates a new sequence that is one element shorter in length to the original.

(defn diff [xs] (map (fn [[a b]] (- b a)) (partition 2 1 xs)))

;; Here we're testing using the flexible iteration function to create the
;; "difference-pyramid". Turns out it's a little bit faster than recursion (22 vs 16 ms).

(defn extrapolated-iteration [xs]
  (->> (seq (iteration identity
                       :initk xs
                       :vf last
                       :kf diff
                       :somef #(not-every? zero? %)))
       reverse
       (reduce + 0)))

(defn part-1 [data]
  (transduce
   (comp (map get-numbers)
         (map extrapolated-iteration))
   +
   (str/split-lines data)))

(= 114 (part-1 test-data))
(comment
  (= 1904165718 (part-1 (slurp "input/2023/d09.txt"))))

;; Part 2 is simply doing the same process, but adding a number to the
;; start of the sequence instead of the end. This time we use recursion:

(defn extrapolated-reverse [xs]
  (if (every? zero? xs)
    0
    (- (first xs) (extrapolated-reverse (diff xs)))))

(defn part-2 [data]
  (transduce
   (comp (map get-numbers)
         (map extrapolated-reverse))
   +
   (str/split-lines data)))

(= 2 (part-2 test-data))
(comment
  (= 964 (part-2 (slurp "input/2023/d09.txt"))))
