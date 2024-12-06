(ns y2024.d06
  (:require
   [medley.core :as medley]
   [advent-of-code-clj.utils :as utils]))

(def test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn find-starting-pos [coord-map]
  (key (medley/find-first #(= \^ (val %)) coord-map)))

(def new-dir {[0 -1] [1 0]
              [1 0] [0 1]
              [0 1] [-1 0]
              [-1 0] [0 -1]})

(defn part-1 [input]
  (let [cm (utils/coord-map (utils/text->matrix input))
        starting-pos (find-starting-pos cm)
        starting-dir [0 -1]]
    (loop [pos starting-pos
           dir starting-dir
           visited-nodes (transient #{})]

      (let [new-pos (mapv + pos dir)
            nvisited (conj! visited-nodes pos)]
        (case (cm new-pos)
          (\. \^) (recur new-pos dir nvisited)
          \# (recur pos (new-dir dir) nvisited)
          nil (count (persistent! nvisited)))))))

(part-1 test-input)

(comment
  (part-1 (slurp "input/2024/input6.txt")))

