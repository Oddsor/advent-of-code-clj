^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2022.d18
  (:require [nextjournal.clerk :as clerk]))

;; # Year 2022 - Day 18

^{::clerk/visibility {:result :hide}}
(def test-data "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

{::clerk/visibility {:result :hide}}

(require '[advent-of-code-clj.utils :as u]
         '[clojure.core.matrix :as m]
         '[clojure.set :as set])

(defn parse [data]
  (m/to-nested-vectors (partition 3 (map parse-long (re-seq #"\d+" data)))))

(defn neighbours [[head & points]]
  (lazy-cat
   (keep (fn [point] (when (>= 1.0 (m/distance head point))
                       [head point]))
         points)
   (when (seq points) (neighbours points))))

(defn part-1 [data]
  (let [points (parse data)
        ;; Each point has six sides!
        total-sides (* (count points) 6)]
    (- total-sides
       ;; For every immediate neighbour we lose two sides
       (* (count (neighbours points)) 2))))

{::clerk/visibility {:result :show}}

^{::clerk/visibility {:code :hide}}
(clerk/example
 (= 64 (part-1 test-data)))

;; Applying the function to our input data we get:

(clerk/code
 '(= 4302 (part-1 (slurp "input/2022/18.txt"))))

{::clerk/visibility {:result :hide}}

(defn adjacents [[x y z]]
  (set (u/adjacent-hv x y z)))

(defn find-valid-neighbours [blocked visited position]
  (for [coord (adjacents position)
        :when (and (not (visited coord))
                   (not (blocked coord)))]
    coord))

(defn fill-area [blocked positions]
  (letfn [(fill-area* [blocked visited positions]
            (let [n-visited (into visited positions)
                  n (distinct (mapcat (partial find-valid-neighbours blocked n-visited) positions))]
              (lazy-cat positions (when (not-empty n)
                                    (fill-area* blocked n-visited n)))))]
    (fill-area* blocked #{} positions)))

(defn add-walls [min-x max-x min-y max-y min-z max-z]
  (set (concat (for [x [min-x max-x]
                     y (range min-y (inc max-y))
                     z (range min-z (inc max-z))]
                 [x y z])
               (for [x (range min-x (inc max-x))
                     y [min-y max-y]
                     z (range min-z (inc max-z))]
                 [x y z])
               (for [x (range min-x (inc max-x))
                     y (range min-y (inc max-y))
                     z [min-z max-z]]
                 [x y z]))))

(defn part-2 [data]
  (let [d (set (parse data))
        [[min-x max-x] [min-y max-y] [min-z max-z]]
        (mapv (juxt m/minimum m/maximum) (apply mapv vector d))
        filled (set (fill-area (set/union (add-walls (- min-x 2) (+ max-x 2)
                                                     (- min-y 2) (+ max-y 2)
                                                     (-  min-z 2) (+ max-z 2))
                                          d)
                               [[(dec min-x) (dec min-y) (dec min-z)]]))
        max-fill (set (fill-area (add-walls (- min-x 2) (+ max-x 2)
                                            (- min-y 2) (+ max-y 2)
                                            (-  min-z 2) (+ max-z 2))
                                 [[(dec min-x) (dec min-y) (dec min-z)]]))
        diff (set/difference max-fill filled)
        total-sides (* (count diff) 6)]
    (- total-sides
           ;; For every immediate neighbour we lose two sides
       (* (count (neighbours diff)) 2))))

{::clerk/visibility {:result :show}}

{::clerk/visibility {:code :hide}}
(clerk/example
 (= 58 (part-2 test-data)))

(clerk/code
 '(= 2492 (part-2 (slurp "input/2022/18.txt"))))