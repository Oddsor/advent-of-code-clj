^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d03
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [advent-of-code-clj.utils :as u]
            [nextjournal.clerk :as clerk]))

;; # Year 2023, day 3

;; Today's task was a struggle, as I didn't quite know how I wanted
;; to model the problem. Also I didn't initially manage to make a
;; performant solution; part 2 took almost two seconds to run!

;; After some twiddling and turning some things on its head I eventually
;; got a decent solution that I'm still not super happy with.

;; The task is basically to traverse a matrix of numbers and symbols,
;; and identify which numbers are adjacent to a symbol:

(def test-data "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

;; ## Part 1

;; For part one, we need to find all numbers that are next to a symbol in
;; the matrix (including adjacent!). Start by converting the text into a matrix:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn text->matrix [text]
  (->> text str/split-lines (mapv #(str/split % #""))))

;; Add some helpers for checking if we're looking at a digit or symbol:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn is-symbol? [x] (re-matches #"[^\d\.]" x))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn is-digit? [character] (re-matches #"\d" character))

;; And a helper for finding all the coordinates that match a digit:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn digit-coordinates [matrix]
  (filter #(is-digit? (get-in matrix %)) (m/index-seq matrix)))

;; Since we chose to just collect digit coordinates, we also need a
;; helper to group digits that are next to each other. Initially I
;; caused a bug by grouping digits on separate lines, so one requirement
;; is that the y-coordinate is different within a group. Overall this
;; part of the code is what I'm least happy with:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn group-number-coordinates [coords]
  (when-let [xs (seq coords)]
    (lazy-seq
     (let [[fst & xss] xs
           grouping (loop [[py px] fst
                           [[ny nx :as n] & xrs] xss
                           acc [fst]]
                      (if (and (= py ny) (= (dec nx) px))
                        (recur n xrs (conj acc n))
                        acc))]
       (cons grouping (group-number-coordinates (drop (count grouping) coords)))))))

(->> test-data text->matrix digit-coordinates group-number-coordinates)

;; We also create a helper to find symbol coordinates:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn symbol-coordinates [matrix]
  (into {} (for [y (range (count matrix))
                 x (range (count (get matrix y)))
                 :let [s (get-in matrix [y x])]
                 :when (is-symbol? s)]
             {[y x] s})))
(-> test-data text->matrix symbol-coordinates)

;; This helper lets us figure out if a given coordinate is adjacent to a symbol:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn adjacent-symbol? [symbol-coords [y x]]
  (some symbol-coords (u/adjacent y x)))

^{:nextjournal.clerk/visibility {:code :hide}}
(let [matrix (-> test-data text->matrix)
       symbol-coords (symbol-coordinates matrix)]
   (clerk/example 
    (adjacent-symbol? symbol-coords [3 7])
    (adjacent-symbol? symbol-coords [3 8])))

;; Now we can generate a filtering function that only finds numbers that
;; are near a symbol, given a list of symbol-coordinates

^{:nextjournal.clerk/visibility {:result :hide}}
(defn filter-number-coords [symbol-coords coords]
  (filter #(some (partial adjacent-symbol? symbol-coords) %) coords))

;; We also need a way to convert coordinates to their respective digits:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn coords->numbers
  ([matrix]
   (map (partial map #(get-in matrix %))))
  ([matrix coords]
   (map (partial map #(get-in matrix %)) coords)))

;; Finally we can combine everything to solve part 1. First we generate a matrix,
;; a map of symbol-coordinates, then collect and group digit coordinates, remove
;; any grouping that is not next to a symbol, convert the groups into a number,
;; and sum up the total:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [matrix (-> data text->matrix)
        symbol-coords (symbol-coordinates matrix)]
    (->> matrix
         digit-coordinates
         group-number-coordinates
         (filter-number-coords symbol-coords)
         (coords->numbers matrix)
         (map (partial apply str))
         (map parse-long)
         (apply +))))

(= 4361 (part-1 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 544433 (part-1 (slurp "input/2023/d03.txt"))))

;; ## Part 2

;; For part 2 it turns out we only need to worry about numbers that are next to a *-symbol,
;; and only where there are exactly two numbers near said symbol!

;; Luckily we generated enough helper code to get us through the problem. Initially my implementation
;; was much slower since I went through all numbers and looked at adjacent coordinates, but a faster
;; version was to turn the problem on its head and look at a symbol's neighbouring coordinates to see
;; if any grouping matched a neighbour-coordinate (after all, there's less *-symbols than digits, and 
;; therefore less coordinates to consider):

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (let [matrix (-> data text->matrix)
        asterisk-coords (into {} (filter (comp #{"*"} val)) (symbol-coordinates matrix))
        number-coords (-> matrix digit-coordinates group-number-coordinates)
        relevant-coords (->> number-coords (filter-number-coords asterisk-coords))]
    (->> asterisk-coords
         keys
         (map (fn [k]
                (let [adj (set (cons k (apply u/adjacent k)))]
                  (->> relevant-coords
                       (filter #(some adj %))
                       (coords->numbers matrix)
                       (map str/join)
                       (map parse-long)))))
         (filter #(= 2 (count %)))
         (map (partial apply *))
         (apply +))))

(= 467835 (part-2 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 76314915 (part-2 (slurp "input/2023/d03.txt"))))