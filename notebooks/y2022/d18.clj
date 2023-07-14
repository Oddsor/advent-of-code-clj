(ns y2022.d18
  (:require [clojure.core.matrix :as m]
            [nextjournal.clerk :as clerk]))

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

^{::clerk/visibility {:result :hide}}
(defn parse [data]
  (mapv m/to-vector (partition 3 (map parse-long (re-seq #"\d+" data)))))

^{::clerk/visibility {:result :hide}}
(defn neighbours [[head & points]]
  (lazy-cat
   (keep (fn [point] (when (>= 1.0 (m/distance head point))
                       [head point])) points)
   (when (seq points) (neighbours points))))

^{::clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [points (parse data)
        ;; Each point has two sides!
        total-sides (* (count points) 6)]
    (- total-sides
       ;; For every immediate neighbour we lose two sides
       (* (count (neighbours points)) 2))))

{::clerk/visibility {:code :hide}}
(clerk/example
 (= 64 (part-1 test-data)))

(clerk/code
 '(= 4302 (part-1 (slurp "input/2022/18.txt"))))

#_(= 58 (part-2 test-data))