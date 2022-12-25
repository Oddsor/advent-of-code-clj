(ns y2022.d23
  (:require [clojure.string :as str]))

(def test-data "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(defn parse [data]
  (let [m (mapv vec (str/split-lines data))]
    (set (for [y (range (count m))
               x (range (count (first m)))
               :when (= \# (get-in m [y x]))]
           [x y]))))

(defn adjacent
  ([[x y] directions]
   (mapv val (filter
              (comp (set directions) key)
              {:E [(inc x) y]
               :W [(dec x) y]
               :SE [(inc x) (inc y)]
               :SW [(dec x) (inc y)]
               :S [x (inc y)]
               :N [x (dec y)]
               :NW [(dec x) (dec y)]
               :NE [(inc x) (dec y)]})))
  ([coords]
   (adjacent coords [:N :S :E :W :NW :NE :SW :SE])))

(def initial-move-order
  [[[:N :NW :NE] [0 -1]]
   [[:SW :S :SE] [0 1]]
   [[:W :NW :SW] [-1 0]]
   [[:E :NE :SE] [1 0]]])

(defn next-move [elf-coordinates move-order [x y :as elf]]
  (if (not-any? (set (adjacent elf)) elf-coordinates)
    elf
    (or (first (keep (fn [[d [dx dy]]]
                       (when (not-any? (set (adjacent elf d)) elf-coordinates)
                         [(+ x dx) (+ y dy)]))
                     move-order))
        elf)))

(defn play-round [[elf-coordinates move-order]]
  (let [from->to (map (fn [coord]
                        [coord (next-move elf-coordinates move-order coord)]) elf-coordinates)]
    [(map (fn [[start end :as x]]
            (if (some #{end} (map second (remove #{x} from->to)))
              start
              end)) from->to)
     (concat (rest move-order) [(first move-order)])]))

(defn count-empty-tiles [st]
  (let [[min-x max-x] (apply (juxt min max) (map first st))
        [min-y max-y] (apply (juxt min max) (map second st))]
    (count (for [x (range min-x (inc max-x))
                 y (range min-y (inc max-y))
                 :when (not ((set st) [x y]))]
             [x y]))))

(defn part-1 [data]
  (let [[st] (nth (iterate play-round [(parse data) initial-move-order])
                  10)]
    (count-empty-tiles st)))

(defn mmap [fun xs]
  (map (partial map fun) xs))

(defn part-2 [data]
  (->> [(parse data) initial-move-order]
       (iterate play-round)
       (partition 2 1)
       (mmap first)
       (take-while #(not= (first %) (second %)))
       count
       inc))

(assert (= 110 (part-1 test-data)))
(assert (= 20 (part-2 test-data)))

(comment
  ;; Runtime: 37 seconds
  (part-1 (slurp "input/2022/23.txt"))
  ;; Runtime: 1 hour
  (part-2 (slurp "input/2022/23.txt")))