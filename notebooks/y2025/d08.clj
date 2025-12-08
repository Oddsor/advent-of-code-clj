(ns y2025.d08
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.math :as math]
    [clojure.math.combinatorics :as combinatorics]
    [loom.alg :as alg]
    [loom.graph :as graph]))

; # 2025, dag 8

; ## Del 1

(def test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defn junction-boxes [input]
  (->> (re-seq #"(\d+),(\d+),(\d+)" input)
       (map (fn [[_ & coords]]
              (mapv Integer/parseInt coords)))))

(def test-junction-boxes (junction-boxes test-input))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (math/sqrt (+ (math/pow (- x2 x1) 2) (math/pow (- y2 y1) 2) (math/pow (- z2 z1) 2))))

(distance (first test-junction-boxes) (second test-junction-boxes))

; Vi kan bruke cominatorics-biblioteket for å finne alle kombinasjonene vi skal undersøke

(-> test-input
    junction-boxes
    (combinatorics/combinations 2)
    count)

(defn part-1 [n-connections input]
  (let [junction-boxes (junction-boxes input)
        graph (->> (combinatorics/combinations junction-boxes 2)
                   (sort-by #(apply distance %))
                   (take n-connections)
                   (apply graph/graph))]
    (->> graph
         alg/connected-components
         (map count)
         (sort >)
         (take 3)
         (reduce *))))

(part-1 10 test-input)

(delay
  (time (part-1 1000 (input/get-input 2025 8))))

; ## Del 2

(defn part-2 [input]
  (let [junction-boxes (junction-boxes input)
        num-boxes (count junction-boxes)
        combinations (->> (combinatorics/combinations junction-boxes 2)
                          (sort-by #(apply distance %)))]
    (loop [[c & r] combinations
           g (graph/graph)]
      (if (nil? c) nil
          (let [ng (graph/add-edges g (vec c))]
            (if (and (= (count (graph/nodes ng)) num-boxes)
                     (= 1 (count (alg/connected-components g))))
              (reduce * (map first c))
              (recur r ng)))))))

(part-2 test-input)

(delay (part-2 (input/get-input 2025 8)))
