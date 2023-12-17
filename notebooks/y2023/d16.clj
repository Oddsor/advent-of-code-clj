(ns y2023.d16 
  (:require [clojure.string :as str]
            [advent-of-code-clj.utils :as u]))

(def test-data ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defn new-dir [op dir]
  (condp = op
    \\ (vec (reverse dir))
    \/ (mapv (partial * -1) (reverse dir))))

(defn move [cm {:keys [dir coord]}]
  (case (cm coord)
    nil nil
    \. [{:coord (mapv + coord dir) :dir dir}]
    \\ [{:coord (mapv + coord (new-dir \\ dir))
         :dir (new-dir \\ dir)}]
    \/ [{:coord (mapv + coord (new-dir \/ dir))
         :dir (new-dir \/ dir)}]
    \- (if (#{[1 0] [-1 0]} dir)
         [{:coord (mapv + coord dir) :dir dir}]
         [(let [ndir (vec (reverse dir))]
            {:coord (mapv + coord ndir) :dir ndir})
          (let [ndir (mapv * (vec (reverse dir)) [-1 -1])]
            {:coord (mapv + coord ndir) :dir ndir})])
    \| (if (#{[0 1] [0 -1]} dir)
         [{:coord (mapv + coord dir) :dir dir}]
         [(let [ndir (vec (reverse dir))]
            {:coord (mapv + coord ndir) :dir ndir}) 
          (let [ndir (mapv * (vec (reverse dir)) [-1 -1])]
            {:coord (mapv + coord ndir) :dir ndir})])))

(defn find-positions [cm current-positions]
  (loop [prev-positions (set current-positions)
         positions current-positions
         i 0]
    (let [new-positions (mapcat (partial move cm) positions)] 
      (if (or (every? prev-positions new-positions) (> i 100000))
        (do (when (> i 100000) (println "Loop protection"))
          prev-positions)
        (recur (into prev-positions new-positions) new-positions
               (inc i))))))

(defn part-1 [data]
  (let [cm (u/coord-map (str/split-lines (str/trim data)))
        positions [{:dir [1 0]
                    :coord [0 0]}]]
    (count (set (filter cm (map :coord (find-positions cm positions)))))))

(= 46 (part-1 test-data))
(comment
  (part-1 (slurp "input/2023/d16.txt")))