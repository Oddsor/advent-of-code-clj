(ns y2022.d18
  (:require [clojure.core.matrix :as m]))

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

(defn parse [data]
  (mapv vec (partition 3 (map parse-long (re-seq #"\d+" data)))))

(defn part-1 [data]
  (let [points (parse data)
        total-sides (* (count points) 6)]
    (- total-sides
       (* (count (set (for [a points
                            b points
                            :when (and
                                   (not= a b)
                                   (>= 1.0 (m/distance a b)))]
                        #{a b})))
           2))))

(comment
  (part-1 (slurp "input/2022/18.txt")))
(assert (= 64 (part-1 test-data)))