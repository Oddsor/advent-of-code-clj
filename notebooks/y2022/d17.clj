^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2022.d17
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; # Part 1

;; > To prove to the elephants your simulation is accurate, they want to know how tall the tower will get after 2022 rocks have stopped (but before the 2023rd rock begins falling). In this example, the tower of rocks will be 3068 units tall.

;; > How many units tall will the tower of rocks be after 2022 rocks have stopped falling?

(def test-data ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

^{:nextjournal.clerk/visibility {:result :hide}}
(defn jet-seq [data]
  (keep {\> 1 \< -1} (seq data)))
(def rocks [:- :+ :J :| :.])
(def rock-cycle (cycle rocks))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-rock [rock-type current-height]
  (let [bottom-y (+ current-height 3)]
    (case rock-type
      :- [[2 bottom-y] [3 bottom-y] [4 bottom-y] [5 bottom-y]]
      :+ [[3 bottom-y] [2 (inc bottom-y)] [3 (inc bottom-y)] [4 (inc bottom-y)] [3 (+ bottom-y 2)]]
      :J [[2 bottom-y] [3 bottom-y] [4 bottom-y] [4 (inc bottom-y)] [4 (+ bottom-y 2)]]
      :| [[2 bottom-y] [2 (inc bottom-y)] [2 (+ bottom-y 2)] [2 (+ bottom-y 3)]]
      :. [[2 bottom-y] [3 bottom-y] [2 (inc bottom-y)] [3 (inc bottom-y)]])))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn move-rock [rock direction visited]
  (let [pushed (mapv (fn [[x y]]
                       [(+ x direction) y]) rock)
        ns (if (every? (fn [[x y]]
                         (and (<= 0 x 6)
                              (not (visited [x y])))) pushed)
             pushed
             rock)
        fall (mapv (fn [[x y]]
                     [x (dec y)]) ns)]
    (if (every? (fn [[x y]]
                  (and (<= 0 y)
                       (not (visited [x y])))) fall)
      [:falling fall]
      [:resting ns])))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn print-board [y-min y-max nodes]
  (println (str/join "\n" (reverse (map (fn [y]
                                          (apply str (map (fn [x] (if (nodes [x y]) \# \.)) (range 7))))
                                        (range y-min y-max))))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-state-after [num-rocks data]
  (loop [[jet & jets] (cycle (jet-seq data))
         resting-rocks 0
         current-height 0
         rock (get-rock (first rock-cycle) 0)
         rock-cycle (next rock-cycle)
         visited #{}]
    (if (= resting-rocks num-rocks)
      {:rested visited :next rock :height (inc current-height)}
      (let [[state moved-rock] (move-rock rock jet visited)
            rock-stuck? (= :resting state)
            new-height (if rock-stuck?
                         (max current-height (apply max (map second moved-rock)))
                         current-height)]
        (recur jets
               (if rock-stuck? (inc resting-rocks) resting-rocks)
               new-height
               (if rock-stuck?
                 (get-rock (first rock-cycle) (inc new-height))
                 moved-rock)
               (if rock-stuck? (next rock-cycle) rock-cycle)
               (if rock-stuck? (into visited moved-rock) visited))))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [num-rocks data]
  (:height (get-state-after num-rocks data)))

^{:nextjournal.clerk/visibility {:result :hide}}
(clerk/example
 (= 3068 (part-1 2022 test-data)))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/code
 '(= 3215 (part-1 2022 (slurp "input/2022/17.txt"))))

;; # Part 2

;; > The elephants are not impressed by your simulation. They demand to know how tall the tower will be after 1000000000000 rocks have stopped! Only then will they feel confident enough to proceed through the cave.

;; > In the example above, the tower would be 1514285714288 units tall!

;; > How tall will the tower be after 1000000000000 rocks have stopped?

;; We need to find a repeating cycle to simplify the calculation.
;; The minimal length we need to run is the number of rocks multiplied by the jet-cycle. For example, in the test data:
(clerk/example
 (* (count test-data) (count rocks)))

;; Define a dynamic var to avoid having to pass the jet-sequence around
(def ^:dynamic *jet-vec* (vec (jet-seq test-data)))

;; We now need to generate a rock/jet index-key to keep track of the unique states
^{:nextjournal.clerk/visibility {:result :hide}}
(defn next-idx [xs idx]
  (mod (inc idx) (count xs)))

;; Make a function that moves a rock and updates the height of the tower.
;; This borrows heavily from the recursive loop in part 1

^{:nextjournal.clerk/visibility {:result :hide}}
(defn iterate-rock [{:keys [jet-idx rock-idx current-height visited]
                     :or {jet-idx 0 rock-idx 0}}]
  (let [rock (get-rock (rocks rock-idx) current-height)]
    (loop [jet jet-idx
           [state moved-rock] (move-rock rock (*jet-vec* jet) visited)]
      (if (= :resting state)
        {:jet-idx (next-idx *jet-vec* jet)
         :rock-idx (next-idx rocks rock-idx)
         :current-height (max current-height (inc (apply max (map second moved-rock))))
         :visited (into visited moved-rock)}
        (let [next-jet (next-idx *jet-vec* jet)]
          (recur next-jet
                 (move-rock moved-rock (*jet-vec* next-jet) visited)))))))

;; This "initializer" builds a tower, but stops once it discovers the loop
;; of rock/jet combinations and returns the a map containing the delta height
;; difference to the next rock

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-cycle []
  (let [min-runs (* (count rocks) (count *jet-vec*))
        max-runs 1000000
        xs (iterate iterate-rock {:current-height 0
                                  :visited #{}})]
    (loop [x (inc min-runs)
           combo-height {}
           combo-vec []]
      (let [state-at-x (nth xs x)
            combo ((juxt :rock-idx :jet-idx) state-at-x)
            height (:current-height state-at-x)
            prev-height (:current-height (nth xs (dec x)))]
        (if (or (contains? combo-height combo) (> x max-runs))
          {:combo-height combo-height
           :combo-cycle combo-vec
           :rocks min-runs
           :min-height (:current-height (nth xs min-runs))}
          (recur (inc x)
                 (assoc combo-height combo (- height prev-height))
                 (conj combo-vec combo)))))))

;; Finally we can calculate the height of the tower. Do this by
;; multiplying the total height increase from each cycle by the
;; amount of cycles we will go through on the way to the top,
;; then manually process the remainder.

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (with-bindings {#'*jet-vec* (vec (jet-seq data))}
    (let [num-rocks 1000000000000
          {:keys [combo-height combo-cycle rocks min-height]} (get-cycle)
          total-cycle-height (apply + (vals combo-height))
          cycle-size (count combo-cycle)
          cycles (long (/ (- num-rocks rocks) cycle-size))
          remainder (- num-rocks (+ (* cycles cycle-size) rocks))]
      (transduce
       (comp 
        (take remainder)
        (map combo-height))
       +
       (+ min-height (* cycles total-cycle-height))
       (cycle combo-cycle)))))

;; Check if our algorithm works with the expected result for the test-set

(= 1514285714288 (part-2 test-data))

;; Now that it works on the dataset, we can apply it to our input

(clerk/code
 '(= 1575811209487
     (part-2 (slurp "input/2022/17.txt"))))
