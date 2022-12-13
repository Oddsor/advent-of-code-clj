(ns y2022.d03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn compartmentalize-backpack [items]
  (split-at (/ (count items) 2) items))

(def char-to-priority
  (merge (zipmap (map char (range (int \a) (inc (int \z)))) (range 1 (inc 26)))
         (zipmap (map char (range (int \A) (inc (int \Z)))) (range 27 (inc 52)))))

(defn common-items [data]
  (mapcat (fn [items]
            (apply set/intersection (map set items)))
          data))

(defn sum-of-item-priorities [data]
  (->> data common-items
       (map char-to-priority)
       (reduce +)))

(defn part-1 [data]
  (->> data
       str/split-lines
       (map compartmentalize-backpack)
       sum-of-item-priorities))

(defn part-2 [data]
  (->> data
       str/split-lines
       (partition 3)
       sum-of-item-priorities))

(comment
  (part-1 (slurp "input/2022/03.txt"))
  (part-2 (slurp "input/2022/03.txt")))