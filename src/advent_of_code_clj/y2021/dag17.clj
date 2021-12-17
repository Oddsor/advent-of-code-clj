(ns advent-of-code-clj.y2021.dag17
  (:require [clojure.core.matrix :as m]))

(defn parse [t]
  (map read-string (re-seq #"-?\d+" t)))

(def test-data (parse "target area: x=20..30, y=-10..-5"))

(defn distance-from-area? [[x y :as proj] [x1 x2 y1 y2 :as box]]
  (if (and (>= x2 x x1)
           (>= y2 y y1))
    0
    (let [distances (map (partial m/distance [x y]) [[x1 y1] [x2 y1]
                                                     [x1 y2] [x2 y2]])]
      (apply min distances))))

(defn step [x y vx vy]
  (let [n [(+ x vx) (+ y vy)
           ((cond (zero? vx) identity
                  (pos? vx) dec
                  (neg? vx) inc) vx)
           (dec vy)]]
    n))

(defn find-hits [[x1 x2 y1 y2 :as data]]
(->> (for [vy (range (min y1 y2) 100)
           vx (range 0 (inc (max x1 x2)))]
       [vx vy])
     (reduce (fn [acc [vx vy]]
               (let [points (take-while #(>= (nth % 1) (min y1 y2))
                                        (iterate (partial apply step)
                                                 [0 0 vx vy]))]
                 (if (some #(= 0 (distance-from-area? % data))
                           (reverse points))
                   (conj acc [vx vy (apply max (map second points))])
                   acc)))
             [])))

(defn find-highest-y [data]
  (->> (find-hits data)
       (sort-by second)
       last last))

(assert (= 45 (find-highest-y test-data)))
(assert (= 112 (count (find-hits test-data))))

(comment
  (def data (parse (slurp "input/y2021/17.txt")))
  (= 1070 (count (find-hits data)))
  (= 2701 (find-highest-y data))
  ;
  )