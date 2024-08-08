(ns y2023.d21
  (:require [advent-of-code-clj.utils :as u]
            [clojure.string :as str]))

(def test-data "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defn part-1 [n data]
  (let [m (u/coord-map (str/split-lines data))
        starting-positions (map first (filter (comp #{\S} last) m))]
    (count (nth (iterate (fn [positions]
                           (distinct (mapcat (fn [pos]
                                               (let [np (apply u/adjacent-hv pos)]
                                                 (filter (comp #{\. \S} m) np))) positions)))
                         starting-positions) n))))

(= 16 (part-1 6 test-data))

(comment
  (part-1 64 (slurp "input/2023/d21.txt")))
