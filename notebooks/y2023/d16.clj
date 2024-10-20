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

;; ## Old implementation

;; Originally I attempted to run a simulation "point by point",
;; but this ended up being very inefficient. This was done using
;; these move-functions:

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

;; ## New implementation

;; To speed up the algorithm, we instead create "beams" that
;; have an entry and exit position, as well as all highlighted
;; areas in the path. This should allow for better deduplication.

^{:nextjournal.clerk/visibility {:result :hide}}
(defn new-dirs [sym dir]
  (case [sym dir]
    ([\- [1 0]] [\\ [0 1]] [\/ [0 -1]] [\. [1 0]])
    [[1 0]]
    ([\- [-1 0]] [\\ [0 -1]] [\/ [0 1]] [\. [-1 0]])
    [[-1 0]]
    ([\| [1 0]] [\| [-1 0]])
    [[0 1] [0 -1]]
    ([\- [0 1]] [\- [0 -1]])
    [[1 0] [-1 0]]
    ([\| [0 -1]] [\\ [-1 0]] [\/ [1 0]] [\. [0 -1]])
    [[0 -1]]
    ([\| [0 1]] [\\ [1 0]] [\/ [-1 0]] [\. [0 1]])
    [[0 1]]))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn calc-beam [coord-map {:keys [dir coord] :as start}]
  (loop [positions [coord]
         current-pos coord]
    (let [new-coord (mapv + current-pos dir)
          new-positions (conj positions new-coord)]
      (case (coord-map new-coord)
        \. (recur new-positions new-coord)
        nil [{:start start
              :end nil
              :positions positions}]
        (mapv (fn [d]
                {:start start
                 :end {:dir d
                       :coord new-coord}
                 :positions new-positions})
              (new-dirs (coord-map new-coord) dir))))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-positions-2 [coord-map {start-coord :coord start-dir :dir}]
  (let [memo-calc (memoize calc-beam)
        starting-beams (mapcat (fn [d]
                                 (memo-calc coord-map {:dir d :coord start-coord}))
                               (new-dirs (coord-map start-coord) start-dir))]
    (loop [visited-beams (set starting-beams)
           current-beams starting-beams]
      (let [new-beams (mapcat (fn [{:keys [end]}]
                                (when end
                                  (memo-calc coord-map end)))
                              current-beams)]
        (if (every? visited-beams new-beams)
          visited-beams
          (recur (into visited-beams new-beams) new-beams))))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn energized-from-position [position coord-map]
  (count (into #{}
               (mapcat :positions)
               (find-positions-2 coord-map position))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [cm (u/coord-map (str/split-lines (str/trim data)))
        position {:dir [1 0]
                  :coord [0 0]}]
    (energized-from-position position cm)))

(= 46 (part-1 test-data))

;; This implementation is still not great, but at least runs at a
;; decent 900 milliseconds.

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (time (= 7608 (part-1 (slurp "input/2023/d16.txt")))))

;; For part 2, we run the same algorithm across several threads using
;; `pmap`, and build a list of starting positions to test.

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

;; This runs at a fairly slow 2 minutes, but at least we get our answer.

(comment
  (time (= 8221 (part-2 (slurp "input/2023/d16.txt")))))
