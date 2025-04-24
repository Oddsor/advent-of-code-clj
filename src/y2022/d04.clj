(ns y2022.d04
  (:require [clojure.set :as set]))

(defn overlaps [[a b c d]]
  (or (<= a c d b)
      (<= c a b d)))

(defn overlaps-at-all [[a b c d]]
  (seq (set/intersection
         (set (range a (inc b)))
         (set (range c (inc d))))))
(defn parse [data]
  (->> data (re-seq #"\d+") (map parse-long) (partition 4)))

(defn part-1 [data]
  (->> (parse data) (filter overlaps) count))

(defn part-2 [data]
  (->> (parse data) (filter overlaps-at-all) count))

(comment
  (= 453 (part-1 (slurp "input/2022/04.txt")))
  (= 919 (part-2 (slurp "input/2022/04.txt"))))