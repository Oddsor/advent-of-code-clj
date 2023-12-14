(ns y2023.d14
  (:require [clojure.string :as str]))

(def test-data "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn roll-north [mut max-y max-x]
  (dotimes [x max-x]
    (dotimes [y max-y]
      (when (= \. (get-in mut [y x]))
        (loop [offset 1]
          (case  (get-in mut [(+ y offset) x])
            \O (do
                 (assoc! (get mut (+ y offset)) x \.)
                 (assoc! (get mut y) x \O))
            (nil \#) nil
            \. (recur (inc offset))))))))

(defn roll-west [mut max-y max-x]
  (dotimes [y max-y]
    (dotimes [x max-x]
      (when (= \. (get-in mut [y x]))
        (loop [offset 1]
          (case  (get-in mut [y (+ x offset)])
            \O (do
                 (assoc! (get mut y) (+ x offset) \.)
                 (assoc! (get mut y) x \O))
            (nil \#) nil
            \. (recur (inc offset))))))))

(defn roll-south [mut max-y max-x]
  (dotimes [x max-x]
    (dotimes [y max-y]
      (let [from (- (dec max-y) y)]
        (when (= \. (get-in mut [from x]))
          (loop [offset 1]
            (let [other-y (- from offset)]
              (case  (get-in mut [other-y x])
                \O (do
                     (assoc! (get mut other-y) x \.)
                     (assoc! (get mut from) x \O))
                (nil \#) nil
                \. (recur (inc offset))))))))))

(defn roll-east [mut max-y max-x]
  (dotimes [y max-y]
    (dotimes [x max-x]
      (let [from (- (dec max-x) x)]
        (when (= \. (get-in mut [y from]))
          (loop [offset 1]
            (let [other-x (- from offset)]
              (case  (get-in mut [y other-x])
                \O (do
                     (assoc! (get mut y) other-x \.)
                     (assoc! (get mut y) from \O))
                (nil \#) nil
                \. (recur (inc offset))))))))))

(defn persist-matrix [txs]
  (dotimes [i (count txs)]
    ;; Persist inner vectors
    (assoc! txs i (persistent! (get txs i))))
  ;; Finally, persist outer vector
  (persistent! txs))

(defn tilt [xs]
  (let [mut (transient (mapv transient xs))
        max-y (count mut)
        max-x (count (get mut 0))]
    (roll-north mut max-y max-x)
    (persist-matrix mut)))

(defn calc-score [xs]
  (apply + (map-indexed (fn [idy row]
                          (* (inc idy) (count (filter #{\O} row)))) (reverse xs))))

(= 136 (calc-score (tilt (mapv vec (str/split-lines test-data)))))

(comment
  (= 109661 (calc-score (tilt (mapv vec (str/split-lines (slurp "input/2023/d14.txt")))))))

(defn tilt-2 [xs]
  (let [txs (transient (mapv transient xs))
        max-y (count txs)
        max-x (count (get txs 0))]
    (roll-north txs max-y max-x)
    (roll-west txs max-y max-x)
    (roll-south txs max-y max-x)
    (roll-east txs max-y max-x)
    (persist-matrix txs)))

;; Starting from the first occurrence, find out how many rotations are needed to reach the end.
;; Then simply perform the last few rotations to get the solution

(defn part-2 [n xs]
  (let [[first-occ last-occ xn] (loop [xs xs
                                       i 1
                                       mst {}]
                                  (let [nxs (tilt-2 xs)]
                                    (if-let [x (get mst nxs)]
                                      [x i nxs]
                                      (recur nxs (inc i) (assoc mst nxs i)))))]
    (nth (iterate tilt-2 xn)
         (mod (- n first-occ) (- last-occ first-occ)))))

(= 64 (calc-score (part-2 1000000000 (mapv vec (str/split-lines test-data)))))

(comment
  (= 90176 (calc-score (part-2 1000000000 (mapv vec (str/split-lines (slurp "input/2023/d14.txt")))))))

