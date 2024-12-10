(ns y2024.d10
  (:require
   [advent-of-code-clj.utils :as utils]
   [medley.core :as medley]
   [advent-of-code-clj.input :as input]))

(def test-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn trailhead-paths [coord-map only-distinct? trailhead]
  (loop [path-heads [trailhead]
         endpoints []]
    (let [npath-heads
          (for [[y x :as node] path-heads
                neighbour-node (utils/adjacent-hv y x)
                :when (= (coord-map neighbour-node) (inc (coord-map node)))]
            neighbour-node)
          {np nil
           ep 9} (group-by (comp #{9} coord-map) npath-heads)]
      (if (empty? np)
        (cond-> (into endpoints ep)
          only-distinct? distinct)
        (recur np (into endpoints ep))))))

(defn trailhead-scores [only-distinct matrix]
  (let [coord-map (utils/coord-map-fixed (utils/emap (comp parse-long str) matrix))
        trailheads (->> coord-map
                        (medley/filter-vals #{0})
                        (map first))]
    (for [trailhead trailheads]
      (count (trailhead-paths coord-map only-distinct trailhead)))))

(= 36 (reduce + (trailhead-scores true (utils/text->matrix test-input))))

(= 531 (reduce + (trailhead-scores true (utils/text->matrix (input/get-input 2024 10)))))

(= 81 (reduce + (trailhead-scores false (utils/text->matrix test-input))))

(= 1210 (reduce + (trailhead-scores false (utils/text->matrix (input/get-input 2024 10)))))
