(ns y2022.d14
  (:require [clojure.string :as str]))

(def test-data "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse [data]
  (map #(partition 2 (map parse-long (re-seq #"\d+" %))) (str/split-lines data)))

(defn max-y [lines]
  (apply max (mapcat #(map second %) lines)))

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

(defn next-position [[x y] m]
  (cond
    (nil? (m [x (inc y)])) [x (inc y)]
    (nil? (m [(dec x) (inc y)])) [(dec x) (inc y)]
    (nil? (m [(inc x) (inc y)])) [(inc x) (inc y)]
    :else nil)
  ;; Better performance than this?
  #_(first (remove #(m %) [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]])))

(defn next-sand-position [max-y m]
  (if (m [500 0])
    nil ;; Starting position is obstructed, don't place anything
    (loop [pos [500 0]]
      (if-let [[_ y :as npos] (next-position pos m)]
        (if (or (nil? npos) (= max-y y))
          npos
          (recur npos))
        pos))))

(defn part-1 [data]
  (let [lines (parse data)
        initial-map (place-rocks lines)
        max-y (max-y lines)]
    (->> (loop [m initial-map]
           (let [sp (next-sand-position max-y m)
                 nm (cond-> m sp (assoc sp :sand))]
             (cond
               (= (last sp) max-y) m
               (= nm m) nm
               :else (recur nm))))
         vals
         (filter #{:sand})
         count)))

(defn part-2 [data]
  (let [lines (parse data)
        initial-map (place-rocks lines)
        max-y (inc (max-y lines))]
    (->> (loop [m initial-map]
           (if-let [sp (next-sand-position max-y m)]
             (recur (assoc m sp :sand))
             m))
         vals
         (filter #{:sand})
         count)))

(assert (= 24 (part-1 test-data)))
(assert (= 93 (part-2 test-data)))
(comment
  (part-1 (slurp "input/2022/14.txt"))
  (part-2 (slurp "input/2022/14.txt")))