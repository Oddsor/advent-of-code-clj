(ns y2023.d11
  (:require [clojure.string :as str]
            [advent-of-code-clj.utils :as u]
            [clojure.walk :as walk]))

;; # Year 2023; Day 11

;; Today's exercise was fairly straightforward; we need to take the ascii-map and expand the columns and
;; rows that are empty. In the test data below, rows 4 and 8 are empty, and columns 3, 5 and 8 are empty:

(def test-data "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

;; ## Part 1

;; We can repeat empty lines by X to add extra rows, then transpose the list of strings so that adding extra
;; lines instead add columns (then transpose back). Not the most elegant solution, but it should work!

^{:nextjournal.clerk/visibility {:result :hide}}
(defn expand [text]
  (let [vertical-expansion (mapcat (fn [line]
                                     (if (every? #{\.} line)
                                       (repeat 2 line)
                                       [line])) (str/split-lines text))
        transposed (apply mapv vector vertical-expansion)
        horizontal-expansion (mapcat (fn [line]
                                       (if (every? #{\.} line)
                                         (repeat 2 line)
                                         [line])) transposed)
        transposed-again (apply mapv vector horizontal-expansion)]
    (str/join "\n" (map str/join transposed-again))))

;; Here we build a set of pairs out of the coordinates. Postwalk is used to convert sets back to vectors;
;; (sets cause issues with destructuring later)

^{:nextjournal.clerk/visibility {:result :hide}}
(defn pairs [coords]
  (walk/postwalk (u/fif set? vec)
                 (set (for [x coords
                            y coords
                            :let [xy (set [x y])]
                            :when (= 2 (count xy))]
                        xy))))

;; The distance between galaxies is just the absolute x-distance plus the absolute y-distance:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn galaxy-distance [[ax ay] [bx by]]
  (+ (abs (- bx ax))
     (abs (- by ay))))

;; Combine all the functions to solve part 1:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (->> data
       expand
       str/split-lines
       u/coord-map
       (keep (fn [[k v]] (when (= \# v) k)))
       pairs
       (map (partial apply galaxy-distance))
       (apply +)))

;; Testing the solution on the test data to see if things are working:

(= 374 (part-1 test-data))

;; It works! Let's use the solution on the input:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 9509330 (->> (slurp "input/2023/d11.txt") expand str/split-lines u/coord-map (keep (fn [[k v]] (when (= \# v) k))) pairs (map (partial apply galaxy-distance)) (apply +))))

;; ## Part 2

;; Obviously the solution above wouldn't work on part 2, where we don't simply add two lines for each empty row/column,
;; but instead are adding 1 000 000! A new solution was necessary.

;; So instead of creating an expanded map, maybe it's easier to simply use the normal map, collect a list of rows and
;; columns that are empty. Then for each galaxy pair we find the rows and columns between them and collect them in lists.

;; Once we have all the rows/columns traversed, we can replace the x/y values with 1 or, if the x/y value corresponds to an
;; empty row/column, replace the value with 1 000 000. This lets us add a "cost" to the steps.

;; Start by collecting empty rows and columns from a piece of text:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn empty-rows-and-cols [text]
  (let [rows (str/split-lines text)]
    {:y (set (keep-indexed (fn [idx row] (when (every? #{\.} row) idx)) rows))
     :x (set (keep-indexed (fn [idx row] (when (every? #{\.} row) idx)) (apply mapv vector rows)))}))

;; Then create a function that takes two coordinates, finds the x/y-indexes traversed, and replaces them with a cost per index:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn expanded-distance [{empty-x :x empty-y :y} expansion [[ax ay] [bx by]]]
  (reduce + (into (mapv (fn [x] (if (empty-x x) expansion 1)) (range (min ax bx) (max ax bx)))
                  (mapv (fn [y] (if (empty-y y) expansion 1)) (range (min ay by) (max ay by))))))

;; We can now combine this into a new solver for part 2, which also works for part 1:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [n data]
  (->> (str/split-lines data)
       u/coord-map
       (keep (fn [[k v]] (when (= \# v) k)))
       pairs
       (map (partial expanded-distance (empty-rows-and-cols data) n))
       (apply +)))

(= 374 (part-2 2 test-data))
(= 1030 (part-2 10 test-data))
(= 8410 (part-2 100 test-data))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 635832237682 (part-2 1000000 (slurp "input/2023/d11.txt"))))