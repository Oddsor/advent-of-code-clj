(ns y2022.d18
  (:require [clojure.core.matrix :as m]))

(defn parse [data]
  (mapv m/to-vector (partition 3 (map parse-long (re-seq #"\d+" data)))))

(defn neighbours [[head & points]]
  (lazy-cat
   (keep (fn [point] (when (>= 1.0 (m/distance head point))
                       [head point])) points)
   (when (seq points) (neighbours points))))

(defn part-1 [data]
  (let [points (parse data)
        ;; Each point has two sides!
        total-sides (* (count points) 6)]
    (- total-sides
       ;; For every immediate neighbour we lose two sides
       (* (count (neighbours points)) 2))))

(comment
  (time (part-1 (slurp "input/2022/18.txt"))))