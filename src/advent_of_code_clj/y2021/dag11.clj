(ns advent-of-code-clj.y2021.dag11
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]
            [clojure.set :as set]))

(defn to-coord-map [xs-of-xses]
  (->> xs-of-xses
       (map-indexed (fn [idy xs]
                      (map-indexed (fn [idx v]
                                     [[idx idy] v])
                                   xs)))
       (transduce cat merge)))

(def parse-nums (comp (partial map read-string)
                      (partial re-seq #"\d")))
(def parse (comp to-coord-map
                 (partial map parse-nums)
                 str/split-lines))

(def test-data (parse  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

(defn to-string
  "Convert coordinate-map back to a string"
  [coord-map]
  (let [coords (keys coord-map)
        max-x (apply max (map first coords))
        max-y (apply max (map second coords))]
    (->> (for [y (range max-y)
               x (range max-x)]
           [x y])
         (map coord-map)
         (partition max-x)
         (map str/join)
         (str/join "\n"))))

(defn adjacent-coords [x y]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y]                   [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn modinc [x] (mod (inc x) 10))

(defn increment-all [coord-map]
  (s/transform
   s/MAP-VALS
   modinc
   coord-map))

(defn zero-coord-set [m]
  (set (keys (filter (comp zero? val) m))))

(defn step [coord-map]
  (loop [previously-flashed #{}
         m (increment-all coord-map)
         newly-flashed (zero-coord-set m)]
    (let [total-flashed (into previously-flashed newly-flashed)
          surrounding-coords (remove total-flashed
                                     (mapcat #(apply adjacent-coords %)
                                             newly-flashed))
          visited (volatile! #{})
          new-m (reduce (fn [m c]
                          (cond-> m
                            (and (contains? m c)
                                 (not (@visited c)))
                            (update c (fn [x]
                                        (let [nx (modinc x)]
                                          (when (zero? nx) (vswap! visited conj c))
                                          nx)))))
                        m surrounding-coords)]
      (if (= new-m m)
        new-m
        (recur total-flashed new-m @visited)))))

(defn not-all-zero? [m]
  (not-every? zero? (vals m)))

(assert (->> (take 101 (iterate step test-data))
             (mapcat vals)
             (filter zero?)
             count
             (= 1656)))

(assert (->> (iterate step test-data)
             (take-while not-all-zero?)
             count
             (= 195)))

(comment
  (def data (parse (slurp "input/y2021/day11-input.txt")))
  ;; Part 1
  (->> (iterate step data)
       (take 101)
       (mapcat vals)
       (filter zero?)
       count
       (= 1642))
  ;; Part 2
  (->> (iterate step data)
       (take-while not-all-zero?)
       count
       (= 320)))