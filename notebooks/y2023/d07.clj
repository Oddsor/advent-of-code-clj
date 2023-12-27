(ns y2023.d07
  (:require [clojure.string :as str]))

(def test-data "32T3K 765
T55J5 684
KK677 28        
KTJJT 220
QQQJA 483")

(defn card->strength [card]
  ({\T 10 \J 11 \Q 12 \K 13 \A 14} card (Character/getNumericValue card)))

(defn hand->strength [hand]
  (let [groups (update-vals (group-by identity hand) count)
        combos (reduce (fn [acc v]
                         (update acc v (fnil inc 0)))
                       {}
                       (vals groups))
        boost (cond
                (combos 5) 6
                (combos 4) 5
                (and (combos 3) (combos 2)) 4
                (combos 3) 3
                (combos 2) (* 1 (combos 2))
                :else 0)]
    (into [boost] (map card->strength hand))))

(defn card->strength-2 [card]
  ({\T 10 \J 1 \Q 12 \K 13 \A 14} card (Character/getNumericValue card)))

(defn hand->strength-2 [hand]
  (let [groups (update-vals (group-by identity hand) count)
        jokers (groups \J 0)
        combos (reduce (fn [acc v]
                         (update acc v (fnil inc 0)))
                       {}
                       (vals groups))
        cards-plus-jokers-larger-than (fn [x]
                                        (some (fn [[k v]]
                                                (>= (if (= \J k)
                                                      v
                                                      (+ v jokers)) x)) groups))
        boost (cond
                (cards-plus-jokers-larger-than 5) 6
                (cards-plus-jokers-larger-than 4) 5
                (or (and (combos 3) (combos 2))
                    (and (= 2 (combos 2)) (= 1 jokers))) 4
                (cards-plus-jokers-larger-than 3) 3
                (= (combos 2) 2) 2
                (cards-plus-jokers-larger-than 2) 1
                :else 0)]
    (into [boost] (map card->strength-2 hand))))

(defn card->bid [line]
  (let [[hand bid] (str/split line #"\s")]
    [hand (parse-long bid)]))

(defn part-1 [data]
  (->> data
       str/split-lines
       (mapv card->bid)
       (sort-by (comp hand->strength first))
       vec
       (map-indexed (fn [idx x] (* (inc idx) (second x))))
       (apply +)))

(= 6440 (part-1 test-data))
(comment
  (= 249390788 (part-1 (slurp "input/2023/d07.txt"))))

(defn part-2 [data]
  (->> data
       str/split-lines
       (mapv card->bid)
       (sort-by (comp hand->strength-2 first))
       vec
       (map-indexed (fn [idx x] (* (inc idx) (second x))))
       (apply +)))

(= 5905 (part-2 test-data))
(comment
  (= 248750248 (part-2 (slurp "input/2023/d07.txt"))))

