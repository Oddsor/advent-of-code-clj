(ns advent-of-code-clj.y2021.dag13
  (:require [clojure.string :as str]))

(defn parse [t]
  (let [sp (partition-by #{""} (str/split-lines t))
        coords (first sp)
        rules (last sp)]
    {:coords (set (map (comp (partial mapv read-string)
                             (partial re-seq #"\d+")) coords))
     :rules (map (fn [x]
                   (let [[c d] (->> x
                                    (re-find #"fold along ((x|y)=(\d+))")
                                    (take-last 2))]
                     [(keyword c) (read-string d)]))
                 rules)}))

(def test-data (parse "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

(defn fold [num at]
  (if (> at num)
    num
    (- at (- num at))))

(defn new-coords [coords [c n :as rule]]
  (set (map (fn [[x y]]
              (case c
                :y [x (fold y n)]
                :x [(fold x n) y]))
            coords)))

(defn to-string [coords]
  (let [[x-range y-range] (map (comp inc (partial apply max))
                               ((juxt (partial map first) (partial map second)) coords))]
    (->> (for [y (range y-range)
               x (range x-range)]
           (if (coords [x y]) "#" "."))
         (partition x-range)
         (map (partial apply str))
         (str/join "\n"))))

(assert (= 17 (count (second (reductions new-coords (:coords test-data) (:rules test-data))))))
(assert (= (to-string (reduce new-coords (:coords test-data) (:rules test-data)))
           "#####
#...#
#...#
#...#
#####"))

(comment
  (def data (parse (slurp "input/y2021/13.txt")))

  (= 720 (count (second (reductions new-coords (:coords data) (:rules data)))))

  (= (to-string (reduce new-coords (:coords data) (:rules data)))
     ".##..#..#.###..###..###...##..#..#.####
#..#.#..#.#..#.#..#.#..#.#..#.#..#....#
#..#.####.#..#.#..#.#..#.#..#.#..#...#.
####.#..#.###..###..###..####.#..#..#..
#..#.#..#.#....#.#..#....#..#.#..#.#...
#..#.#..#.#....#..#.#....#..#..##..####"))
