(ns y2023.d13
  (:require [clojure.string :as str]))

(def test-data "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn all-equal? [v idx]
  (let [idy (inc idx)
        min-x -1
        max-x (count v)]
    (if (>= idx (dec max-x))
      false
      (loop [offset 0
             matches? true]
        (if (or (not matches?)
                (= min-x (- idx offset))
                (= max-x (+ idy offset)))
          matches?
          (let [nmatches (every? true?
                                 (map = (v (- idx offset)) (v (+ idy offset))))]
            (recur (inc offset) nmatches)))))))

(defn reflection-number [pattern]
  (let [v (mapv vec (str/split-lines pattern))
        tv (vec (apply mapv vector v))]
    (or (reduce (fn [_ idx]
                  (when (all-equal? v idx)
                    (reduced (* 100 (inc idx)))))
                nil (range 0 (count v)))
        (reduce (fn [_ idx]
                  (when (all-equal? tv idx)
                    (reduced (inc idx))))
                nil (range 0 (count tv))))))

(= 5 (reflection-number (first (str/split test-data #"\n\n"))))
(= 400 (reflection-number (second (str/split test-data #"\n\n"))))
(= 405 (reduce + (map reflection-number (str/split test-data #"\n\n"))))

(comment
  (= 37561 (reduce + (map reflection-number (str/split (slurp "input/2023/d13.txt") #"\n\n")))))