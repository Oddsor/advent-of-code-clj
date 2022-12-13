(ns advent-of-code-clj.y2021.d20
  (:require [clojure.string :as str]
            [advent-of-code-clj.utils :as u]))

(defn parse [t]
  (let [[iha grid] (->> t
                        str/split-lines
                        (partition-by #{""})
                        (remove #{(list "")}))]
    {:iha (into {} (map-indexed (fn [idx x] [idx x])) (first iha))
     :grid (u/coord-map grid)}))

(def test-data (parse "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"))

(defn enhance-pos [odd? iha coord-map coord]
  (let [adj (sort (fn [[ax ay] [bx by]]
                    (let [y (compare ay by)]
                      (if (zero? y)
                        (compare ax bx)
                        y)))
                  (conj (apply u/adjacent coord) coord))]
    (->> adj
         (map #(coord-map % (if odd? (iha 0) \.)))
         (replace {\. 0
                   \# 1})
         (apply str "2r")
         read-string
         iha)))

(defn enhance-image [odd? iha grid]
  (let [[min-y max-y] (apply (juxt min max) (map second (keys grid)))
        [min-x max-x] (apply (juxt min max) (map first (keys grid)))]
    (->> (for [y (range (- min-y 1) (+ max-y 2))
               x (range (- min-x 1) (+ max-x 2))]
           [x y])
         (reduce (fn [acc c]
                   (assoc acc c (enhance-pos odd? iha grid c))) {}))))
(defn enhance-times [n iha grid]
  (reduce (fn [g flip]
            (enhance-image flip iha g)) grid (take n (cycle [false true]))))

(defn print-image [grid]
  (let [[min-y max-y] (apply (juxt min max) (map second (keys grid)))
        [min-x max-x] (apply (juxt min max) (map first (keys grid)))]
    (println (str/join "\n" (map (fn [y]
                                   (apply str (map (fn [x]
                                                     (grid [x y])) (range min-x (inc max-x)))))
                                 (range min-y (inc max-y)))))))

(comment
  (assert (= 35 (let [{:keys [iha grid]} test-data]
                  (->> grid
                       (enhance-times 2 iha)
                       vals
                       (filter #{\#})
                       count))))

  (assert (= 3351 (let [{:keys [iha grid]} test-data]
                    (->> grid
                         (enhance-times 50 iha)
                         vals
                         (filter #{\#})
                         count)))))

(comment
  (let [{:keys [iha grid]} test-data]
    (->> grid
         (enhance-times 2 iha)
         print-image)))

(comment
  (let [{:keys [iha grid]} (->> "input/y2021/20.txt"
                                slurp
                                parse)]
    (= 5097 (->> grid
                 (enhance-times 2 iha)
                 vals
                 (filter #{\#})
                 count)))

  (let [{:keys [iha grid]} (->> "input/y2021/20.txt"
                                slurp
                                parse)]
    (= 17987 (->> grid
                  (enhance-times 50 iha)
                  vals
                  (filter #{\#})
                  count)))
  ;;
  )