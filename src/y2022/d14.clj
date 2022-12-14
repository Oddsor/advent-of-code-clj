(ns y2022.d14
  (:require [clojure.string :as str]
            [criterium.core :as crit]))

(def test-data "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse [data]
  (map #(partition 2 (map parse-long (re-seq #"\d+" %))) (str/split-lines data)))

(defn max-y [lines]
  (apply max (mapcat #(map second %) lines)))

(defn line->coordinates [line]
  (->> (partition 2 1 line)
       (mapcat (fn [[[ax ay] [bx by]]]
                 (cond
                   (= ax bx) (for [y (range (min ay by) (inc (max ay by)))]
                               [ax y])
                   (= ay by) (for [x (range (min ax bx) (inc (max ax bx)))]
                               [x ay]))))
       distinct))

(defn place-rocks [lines]
  (->> lines 
       (mapcat line->coordinates)
       (reduce #(assoc %1 %2 :rock) {})))

(defn place-floor [max-y]
  (let [floor-length (* max-y 6)]
    (into {} (for [dx (range floor-length)]
               [[(+ (- 500 (/ floor-length 2)) dx) max-y] :floor]))))

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

(defn place-sand [max-y state]
  (lazy-seq
   (cons state
         (let [sp (next-sand-position max-y state)
               ns (cond-> state sp (assoc sp :sand))]
           (when-not (or (= (last sp) max-y) (= ns state))
             (place-sand max-y ns))))))

(defn part-1 [data]
  (let [lines (parse data)]
    (->> (place-rocks lines)
         (place-sand (max-y lines))
         count
         dec)))

(defn part-2 [data]
  (let [lines (parse data)
        max-y (+ (max-y lines) 2)]
    (->> (merge (place-rocks lines)
                (place-floor max-y))
         (place-sand max-y)
         count
         dec)))

(assert (= 24 (part-1 test-data)))
(assert (= 93 (part-2 test-data)))
(comment
  (crit/quick-bench (part-1 (slurp "input/2022/14.txt")))
  (crit/quick-bench (part-2 (slurp "input/2022/14.txt"))))