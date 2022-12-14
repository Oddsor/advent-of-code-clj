(ns y2022.d14 
  (:require [clojure.string :as str]))

(def test-data "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse [data]
  (map #(partition 2 (map parse-long (re-seq #"\d+" %))) (str/split-lines data)))

(defn map-dimensions [lines]
  (let [y-min 0
        y-max (apply max (mapcat #(map second %) lines))
        x-min (dec (apply min (mapcat #(map first %) lines)))
        x-max (inc (apply max (mapcat #(map first %) lines)))]
    [y-min y-max x-min x-max]))

(defn line->coordinates [line]
  (distinct
   (mapcat (fn [[[ax ay] [bx by]]]
             (cond
               (= ax bx) (for [y (range (min ay by) (inc (max ay by)))]
                           [ax y])
               (= ay by) (for [x (range (min ax bx) (inc (max ax bx)))]
                           [x ay]))) 
           (partition 2 1 line))))

(defn place-rocks [lines]
  (reduce (fn [acc line] 
            (into acc (map vector (line->coordinates line) (repeat :rock)))) {} lines))


;; Map with coordinate->rock
(place-rocks (parse test-data))