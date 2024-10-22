^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d13)

;; # Year 2023 D13

;; I previously solved part 1 of this task a lot less elegantly,
;; and eventually ended up looking up other solutions to guide me
;; in the right direction.

;; Initially I thought I should try to create permutations of the
;; pattern to loop through, but learned that a more elegant method
;; would be to just count the mismatches in the reflection.
;; Part 1 will then be solved by finding a reflection with zero
;; mismatches (an actual reflection), and part 2 will be solved by
;; finding exactly one error.

;; Let's begin:

(def test-data "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

;; We need a helper to create matrices:

^{:nextjournal.clerk/visibility {:result :hide}}
(require '[clojure.string :as str])
^{:nextjournal.clerk/visibility {:result :hide}}
(defn to-matrix [pattern]
  (mapv vec (str/split-lines pattern)))
^{:nextjournal.clerk/visibility {:result :hide}}
(defn matrices [input]
  (map to-matrix (str/split input #"\n\n")))

(def sample-matrix (first (matrices test-data)))

;; And another helper to invert the matrix:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn invert [matrix] (vec (apply mapv vector matrix)))

(invert sample-matrix)

;; And another helper to create a cut in the pattern:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn cut-and-reflect-at [idx matrix]
  (let [[upper lower] (split-at idx matrix)]
    [(reverse upper) lower]))

;; Cutting the first pattern at index 3 looks like it creates a 
;; reflection at first glance, but don't have the algorithm to
;; determine it yet!

(cut-and-reflect-at 3 sample-matrix)

;; The task specifies that we only test the reflection that we see,
;; which is nice since the `map`-function lets us pass in multiple
;; collections, and stops when we reach the end of the smaller list.

;; For example, adding two lists only adds up to the length of the shortest list:
(map + [1 2 3 4] [5 6])

;; So we can make a function that counts the number of discrepancies
;; per line and then sums up the total discrepancies between two matrices:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn num-differences [coll1 coll2]
  (reduce + (map (fn [line1 line2]
                   (count (sequence (comp (map =) (filter false?)) line1 line2)))
                 coll1 coll2)))

;; With this we can see that there wasn't actually a reflection after all:

(apply num-differences (cut-and-reflect-at 3 sample-matrix))

;; So let's see where the reflection is:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn differences-at-indexes [matrix]
  (for [idx (range 1 (count matrix))]
    {:idx idx
     :differences (apply num-differences (cut-and-reflect-at idx matrix))}))

(differences-at-indexes sample-matrix)
;; None on the horizontal axis, maybe on the vertical axis?
(differences-at-indexes (invert sample-matrix))

;; Looks like there's a reflection at index 5 on the vertical reflection!
;; Now we can combine our functions, after adding a score-calculation:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn calculate-score [inverted? idx]
  (if inverted?
    idx
    (* 100 idx)))

;; And a function to locate the reflection, given the amount of differences we allow:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn index-of-reflection [num-differences matrix]
  (first (keep (fn [{:keys [idx differences]}]
                 (when (= num-differences differences)
                   idx))
               (differences-at-indexes matrix))))

;; These two combine to find the score for a pattern:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn calculate-score-for-pattern [num-differences matrix]
  (let [inverted (invert matrix)]
    (if-let [idx-of-matrix (index-of-reflection num-differences matrix)]
      (calculate-score false idx-of-matrix)
      (let [idx-of-inverted (index-of-reflection num-differences inverted)]
        (calculate-score true idx-of-inverted)))))

(calculate-score-for-pattern 0 sample-matrix)

;; Finally, to sum up the score of all patterns:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn solve [differences test-data]
  (transduce (map (partial calculate-score-for-pattern differences))
             + (matrices test-data)))

;; Which gives the following for part 1:
(= 405 (solve 0 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment (= 37561 (solve 0 (slurp "input/2023/d13.txt"))))
;; And the following for part 2:
(= 400 (solve 1 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment (= 31108 (solve 1 (slurp "input/2023/d13.txt"))))
