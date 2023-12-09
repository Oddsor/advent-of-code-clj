(ns y2023.d09
  (:require [clojure.string :as str]))

(def test-data "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn get-numbers [line] (mapv parse-long (re-seq #"-?\d+" line)))
(defn diff [xs] (map (fn [[a b]] (- b a)) (partition 2 1 xs)))

(defn extrapolated [xs]
  (if (every? zero? xs)
    0
    (+ (last xs) (extrapolated (diff xs)))))

(defn part-1 [data]
  (transduce
   (comp (map get-numbers)
         (map extrapolated))
   +
   (str/split-lines data)))

(= 114 (part-1 test-data))
(comment
  (= 1904165718 (part-1 (slurp "input/2023/d09.txt"))))

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
