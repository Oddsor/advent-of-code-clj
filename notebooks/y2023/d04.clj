(ns y2023.d04 
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def test-data "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn card-points [matches]
  (->> (range matches)
       (reduce (fn [acc _]
                 (if (zero? acc) 1 (* acc 2))) 0)))

(defn winning-numbers [line]
  (let [[card winning-number-string drawn-number-string] (str/split line #"[:\|]")
        winning-numbers (set (map parse-long (re-seq #"\d+" winning-number-string)))
        drawn-numbers (set (map parse-long (re-seq #"\d+" drawn-number-string)))]
    {(parse-long (re-find #"\d+" card))
     (count (set/intersection winning-numbers drawn-numbers))}))

(= 13 (->> test-data str/split-lines (into {} (map winning-numbers)) vals (map card-points) (apply +)))
(comment
  (= 22674 (->> (slurp "input/2023/d04.txt") str/split-lines (into {} (map winning-numbers)) vals (map card-points) (apply +))))

(defn part-2 [data]
  (let [card->matches (->> data
                           str/split-lines
                           (into {} (map winning-numbers)))
        max-card (->> card->matches keys (apply max))
        card-numbers (range 1 (inc max-card))]
    (->> (reduce
          (fn [acc card-num]
            (let [winning-numbers (card->matches card-num)
                  inc-amount (acc card-num)
                  next-num (inc card-num)]
              (if (zero? inc-amount)
                acc
                (merge-with +
                            acc
                            (zipmap (range next-num (inc (min max-card (+ card-num winning-numbers))))
                                    (repeat inc-amount))))))
          (zipmap card-numbers (repeat 1))
          card-numbers)
         vals
         (apply +))))

(= 30 (part-2 test-data))

(comment
  (part-2 (slurp "input/2023/d04.txt")))