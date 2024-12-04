(ns y2024.d04
  (:require
   [advent-of-code-clj.utils :as utils]))

(def test-data (utils/coord-map "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"))

(defn coord-line [length [x y]]
  [; Down
   (for [i (range length)]
     [x (+ y i)])
   ; Up
   (for [i (range length)]
     [x (- y i)])
   ; Right
   (for [i (range length)]
     [(+ x i) y])
   ; Left
   (for [i (range length)]
     [(- x i) y])
   ; Down right
   (for [i (range length)]
     [(+ x i) (+ y i)])
   ; Up left
   (for [i (range length)]
     [(- x i) (- y i)])
   ; Down left
   (for [i (range length)]
     [(- x i) (+ y i)])
   ; Up right
   (for [i (range length)]
     [(+ x i) (- y i)])])

(coord-line 4 [0 0])

(defn find-xmas [coord-map coord]
  (let [seqs (coord-line 4 coord)]
    (->> seqs
         (map (fn [xs]
                (map coord-map xs)))
         (filter #{["X" "M" "A" "S"]})
         count)))

(defn part-1 [input]
  (let [coord-map input
        x-es (filter (comp #{"X"} val) coord-map)]
    (->> x-es
         (map #(find-xmas coord-map (key %)))
         (reduce +))))

(= 18 (part-1 test-data))

(comment
  (part-1 (utils/coord-map (slurp "input/2024/input4.txt"))))

(defn find-x-mas [coord-map [x y]]
  (let [diagonals [[[(dec x) (dec y)] [x y] [(inc x) (inc y)]]
                   [[(dec x) (inc y)] [x y] [(inc x) (dec y)]]]]
    (->> diagonals
         (map #(String/join "" (map coord-map %)))
         (filter #{"MAS" "SAM"})
         count
         (= 2))))

(defn part-2 [input]
  (let [coord-map input
        a-s (filter (comp #{"A"} val) coord-map)]
    (->> a-s
         (filter #(find-x-mas coord-map (key %)))
         count)))

(= 9 (part-2 test-data))

(comment
  (part-2 (utils/coord-map (slurp "input/2024/input4.txt"))))
