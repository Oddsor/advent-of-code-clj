(ns y2023.d24
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def test-data "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(defn hailstone [line]
  (let [[x y z dx dy dz] (map parse-long (re-seq #"-?\d+" line))]
    {:position [x y z]
     :velocity [dx dy dz]}))

;; Stolen/modified from this solution over at rosetta code:
;; https://rosettacode.org/wiki/Find_the_intersection_of_two_lines#Clojure

(defn compute-line-2d [[x y] [dx dy]]
  (let [m (/ dy dx)]
    {:slope  m
     :offset (- y (* m x))}))

(defn intercept [line1 line2]
  (try
    (let [x (/ (- (:offset line1) (:offset line2))
               (- (:slope  line2) (:slope  line1)))]
      {:x x
       :y (+ (* (:slope line1) x)
             (:offset line1))})
    (catch Exception _
      ;; Does not intersect
      nil)))

(defn vector-distance [[ax ay] [bx by]]
  (math/sqrt (+ (math/pow (- bx ax) 2) (math/pow (- by ay) 2))))

(defn past? [{[bx by] :position [dx dy] :velocity}
             {:keys [x y]}]
  (> (vector-distance (mapv + [bx by] [dx dy]) [x y])
     (vector-distance [bx by] [x y])))

(defn add-line [{:keys [position velocity] :as hailstone}]
  (assoc hailstone :line (compute-line-2d position velocity)))

(defn part-1 [min-x max-x data]
  (let [hailstones (into [] (comp (map hailstone)
                                  (map add-line))
                         (str/split-lines data))
        possible-intersections (for [i (range (count hailstones))]
                                 (for [j (range (inc i) (count hailstones))]
                                   (let [a (get hailstones i)
                                         b (get hailstones j)]
                                     {:a a :b b
                                      :intercept (intercept (:line a) (:line b))})))
        in-test-area? (fn [{{:keys [x y]} :intercept}]
                        (and (>= max-x x min-x)
                             (>= max-x y min-x)))
        in-past? (fn [{:keys [a b intercept]}]
                   (or (past? a intercept)
                       (past? b intercept)))]
    (count (sequence (comp cat
                           (filter :intercept)
                           (filter (every-pred (complement in-past?) in-test-area?)))
                     possible-intersections))))

(= 2 (part-1 7 27 test-data))
(comment
  (= 17867 (part-1 200000000000000 400000000000000 (slurp "input/2023/d24.txt"))))
