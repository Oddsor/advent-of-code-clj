(ns advent-of-code-clj.y2021.d15
  (:require [clojure.string :as str]
            [advent-of-code-clj.utils :as u]
            [ubergraph.alg :as ua]))

(defn parse [t]
  (map (fn [line]
         (map read-string (re-seq #"\d" line)))
       (str/split-lines t)))

(def test-data (parse "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"))

(defn calc-cost-to-end [data]
  (let [coord-map (u/coord-map data)
        destination [(dec (count (first data))) (dec (count data))]]
    (-> (ua/shortest-path (fn [[x y]]
                            (eduction
                             (filter (fn [[x y]]
                                       (and (> (count data) x -1)
                                            (> (count data) y -1))))

                             (map (fn [coord]
                                    {:dest coord :weight (coord-map coord)}))
                             (u/adjacent-hv x y)))
                          {:start-node [0 0]
                           :end-node destination
                           :cost-attr :weight})
        ua/cost-of-path)))

(defn incnum [num]
  (let [new-num (inc num)]
    (if (> new-num 9)
      1
      new-num)))

(defn inc-line [line]
  (map incnum line))

(defn apply-times [n f xs]
  (->> xs
       (iterate f)
       (take n)
       (mapcat identity)))

(defn expand-rows [data]
  (->> (map (partial apply-times 5 inc-line) data)
       (apply-times 5 (partial map inc-line))))

(assert (= 40 (calc-cost-to-end test-data)))
(assert (= 315 (calc-cost-to-end (expand-rows test-data))))

(comment
  (def data (parse (slurp "input/y2021/15.txt")))
  ;; Part 1
  (time (= 621 (calc-cost-to-end data)))
  ;; Part 2
  (time (= 2904 (calc-cost-to-end (expand-rows data))))
  ;;
  )