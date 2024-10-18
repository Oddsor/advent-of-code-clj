^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d16
  (:require [clojure.string :as str]
            [advent-of-code-clj.utils :as u]))

;; # Year 2023, day 16

;; In today's task, a lightray passes through various mirrors/splitters,
;; and we try to figure out how many coordinates are illuminated/energized.

;; Our test-data shows a map consisting of open spaces and mirrors (/, \) and
;; splitters (|, -)

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

^{:nextjournal.clerk/visibility {:result :hide}}
(defn new-dir [op dir]
  (condp = op
    \\ (vec (reverse dir))
    \/ (mapv (partial * -1) (reverse dir))))

^{:nextjournal.clerk/visibility {:result :hide}}
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

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-positions [cm current-positions]
  (loop [prev-positions (set current-positions)
         positions current-positions
         i 0]
    (let [new-positions (into [] (mapcat #(move cm %)) positions)]
      (if (or (every? prev-positions new-positions)
              (> i 100000))
        (do (when (> i 100000)
              (println "Loop protection"))
            prev-positions)
        (recur (into prev-positions new-positions) new-positions
               (inc i))))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn energized-from-position [position coord-map]
  (count (into #{}
               (comp (map :coord) (filter coord-map))
               (find-positions coord-map [position]))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [cm (u/coord-map (str/split-lines (str/trim data)))
        position {:dir [1 0]
                  :coord [0 0]}]
    (energized-from-position position cm)))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (clojure.repl.deps/add-lib)
  (require '[criterium.core :as crit])
  (crit/quick-bench (part-1 test-data)))

(= 46 (part-1 test-data))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/profile (part-1 (slurp "input/2023/d16.txt")))
  (prof/serve-ui 8080)
  (time (= 7608 (part-1 (slurp "input/2023/d16.txt")))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (let [cm (u/coord-map (str/split-lines (str/trim data)))
        [max-x max-y] (last (sort (keys cm)))
        positions (concat (for [y (range (inc max-y))]
                            {:dir [1 0]
                             :coord [0 y]})
                          (for [y (range (inc max-y))]
                            {:dir [-1 0]
                             :coord [max-x y]})
                          (for [x (range (inc max-x))]
                            {:dir [0 1]
                             :coord [x 0]})
                          (for [x (range (inc max-x))]
                            {:dir [0 -1]
                             :coord [x max-y]}))]
    (apply max (pmap #(energized-from-position % cm) positions))))

(= 51 (part-2 test-data))

(comment
  (crit/quick-bench (part-2 test-data))
  (time (= 7608 (part-2 (slurp "input/2023/d16.txt")))))
