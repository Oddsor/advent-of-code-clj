(ns y2024.d12
  (:require
   [advent-of-code-clj.input :as input]
   [advent-of-code-clj.utils :as utils]))

; # 2024, dag 12

(def test-input "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(utils/text->matrix test-input)

(utils/coord-map-fixed (utils/text->matrix test-input))

; ## Del 1

(defn find-neighbours [predicate node]
  (for [neighbour (apply utils/adjacent-hv node)
        :when (predicate neighbour)]
    neighbour))

(defn find-region [coord-map node]
  (sequence cat
            (utils/breadth-search
             (fn [node]
               (find-neighbours #(= (coord-map node)
                                    (coord-map %))
                                node))
             [node])))

(defn find-regions [coord-map]
  (loop [visited #{}
         regions []
         [node & remaining] (map first coord-map)]
    (cond
      (nil? node) regions
      (visited node) (recur visited regions remaining)
      :else (let [region (find-region coord-map node)]
              (recur
               (into visited region)
               (conj regions region)
               remaining)))))

(defn circumference [nodes]
  (let [nodeset (set nodes)]
    (reduce + (for [node nodes
                    :let [neighbours (filter nodeset
                                             (apply utils/adjacent-hv node))]]
                (- 4 (count neighbours))))))

(defn sum-area-and-circumference [nodes]
  (let [area (count nodes)]
    (* area (circumference nodes))))

(defn part-1 [input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        regions (find-regions coord-map)]
    (transduce (map sum-area-and-circumference) + regions)))

(= 1930 (part-1 test-input))

(time (part-1 (input/get-input 2024 12)))
