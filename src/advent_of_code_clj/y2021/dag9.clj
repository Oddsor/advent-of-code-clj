(ns advent-of-code-clj.y2021.dag9
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
       (map (fn [xs]
              [xs (get-in data (reverse xs) nil)]))
       (into {})))

(defn find-minima [data]
  (assert (vector-matrix? data) "Må være vektormatrise!")
  (for [y (range (count data))
        x (range (count (first data)))
        :let [height (get-in data [y x])
              adjacents (->> (adjacent x y data)
                             vals
                             (remove nil?))]
        :when (and (= height (apply min height adjacents))
                   (not-any? #{height} adjacents))]
    [x y (inc height)]))

(def valid-basin-height? (complement (some-fn #{9} nil?)))

(defn find-adjacent-higher [x y data]
  (let [height (get-in data [y x])
        higher-adjacents (->> (adjacent x y data)
                              (filter (comp valid-basin-height? val))
                              (filter #(> (val %) height)))]
    (cons [x y height]
          (mapcat (fn [[x y]]
                    (find-adjacent-higher x y data))
                  (keys higher-adjacents)))))

(assert (= 15 (apply + (map last (find-minima test-data)))))

(assert (= 1134 (->> test-data
                     find-minima
                     (map (fn [[x y]]
                            (count (set (find-adjacent-higher x y test-data)))))
                     sort
                     (take-last 3)
                     (apply *))))

(comment
  ;; Part 1
  (->> (parse-lines (slurp "input/y2021/day9-input.txt"))
       find-minima
       (map last)
       (apply +))
  ;; Part 2
  (let [data (parse-lines (slurp "input/y2021/day9-input.txt"))]
    (->> data
         find-minima
         (map (fn [[x y]]
                (count (set (find-adjacent-higher x y data)))))
         sort
         (take-last 3)
         (apply *))))