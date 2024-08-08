(ns y2023.d23
  (:require [clojure.string :as str]
            [ubergraph.alg :as ua]
            [advent-of-code-clj.utils :as u]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)

(def test-data "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(def dir-vector
  {\> [1 0]
   \< [-1 0]
   \^ [0 -1]
   \v [0 1]})

(def g (apply ubergraph.core/digraph
              (let [m (u/coord-map (str/split-lines test-data))]
                (mapcat (fn [[k v]]
                          (case v
                            (\# nil) nil
                            \. (keep (fn [x]
                                       (when (m x)
                                         [k x {:distance 1 :cost -1}]))
                                     (apply u/adjacent-hv k))
                            (\> \< \v \^)
                            [[k (mapv + k (dir-vector (m k))) {:distance 1 :cost -1}]]))
                        m))))

(let [cm (u/coord-map (str/split-lines test-data))
      next-nodes (fn [node]
                   (case (cm node)
                     (\# nil) nil
                     \. (mapv (fn [x] {:dest x
                                       :cost 1})
                              (apply u/adjacent-hv node))
                     (\> \< \v \^) [{:dest (mapv + node (dir-vector (cm node)))
                                     :cost 1}]))]
  (ua/shortest-path next-nodes
                    {:start-node [1 0]
                     :cost-attr :cost
                     :traverse true
                     :min-cost 74
                     :end-node [(dec (apply max (map first (keys cm))))
                                (apply max (map second (keys cm)))]}))

(map (fn [x] (mapv + x [1 1])) (keys (u/coord-map (str/split-lines (slurp "input/2023/d23.txt")))))