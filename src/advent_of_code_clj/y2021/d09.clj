(ns advent-of-code-clj.y2021.d09
  (:require [clojure.string :as str]))

(defn parse-nums [text]
  (mapv read-string (re-seq #"\d" text)))
(defn parse-lines [text]
  (mapv parse-nums (str/split-lines text)))

(def test-data (parse-lines "2199943210
3987894921
9856789892
8767896789
9899965678"))

(defn vector-matrix? [xs]
  (and (vector? xs)
       (every? vector? xs)))

(defn adjacent [x y data]
  (assert (vector-matrix? data)
          "Må være vektormatrise for at get-in skal fungere!")
  (->> [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
       (keep (fn [xs]
               (when-let [v (get-in data (reverse xs) nil)]
                 [xs v])))
       (into {})))

(defn find-minima [data]
  (assert (vector-matrix? data) "Må være vektormatrise!")
  (for [y (range (count data))
        x (range (count (first data)))
        :let [height (get-in data [y x])
              adjacents (vals (adjacent x y data))]
        :when (and (= height (apply min height adjacents))
                   (not-any? #{height} adjacents))]
    [x y (inc height)]))

(def peak? #{9})

(defn higher-adjacents [x y data]
  (let [height (get-in data [y x])]
    (->> (adjacent x y data)
         (remove (comp peak? val))
         (filter #(> (val %) height))
         keys)))

(defn expand-outward [plots data]
  (lazy-seq
    (if-let [xs (seq (mapcat (fn [[x y]]
                               (higher-adjacents x y data)) plots))]
      (cons plots (expand-outward xs data))
      [plots])))

(defn collect-plots [plot-groups]
  (set (mapcat identity plot-groups)))

(assert (= 15 (apply + (map last (find-minima test-data)))))

(assert (= 1134 (->> test-data
                     find-minima
                     (map (fn [xs]
                            (-> (expand-outward [xs] test-data)
                                collect-plots
                                count)))
                     sort
                     (take-last 3)
                     (apply *))))

(comment
  ;; Part 1
  (->> (parse-lines (slurp "input/y2021/09.txt"))
       find-minima
       (map last)
       (apply +)
       (= 444))
  ;; Part 2
  (let [data (parse-lines (slurp "input/y2021/09.txt"))]
    (->> data
         find-minima
         (map (fn [plot]
                (-> (expand-outward [plot] data)
                    collect-plots
                    count)))
         sort
         (take-last 3)
         (apply *)
         (= 1168440))))