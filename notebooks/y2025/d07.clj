(ns y2025.d07
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.utils :as utils]
    [medley.core :as medley]))

(def test-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(def test-map (->> test-input utils/text->matrix utils/coord-map-fixed))

(defn get-initial-lasers [coord-map]
  (keys (medley/filter-vals #{\S} coord-map)))

(get-initial-lasers test-map)

(defn move-south [[y x]] [(inc y) x])

(->> test-map get-initial-lasers (map move-south))

(defn step [coord-map {:keys [laser-tips splits]}]
  (let [moved-lasers (filter coord-map (map move-south laser-tips))
        {hits true non-hits false} (group-by (comp boolean #{\^} coord-map)
                                             moved-lasers)
        split-beams (mapcat (fn [[y x]]
                              [[y (dec x)] [y (inc x)]]) hits)]
    {:splits (+ splits (count hits))
     :laser-tips (into (set non-hits) (filter coord-map)
                       split-beams)}))

(let [initial-lasers (->> test-map get-initial-lasers)]
  (step test-map (step test-map {:splits 0 :laser-tips initial-lasers})))

(defn part-1 [input]
  (let [coord-map (->> input utils/text->matrix utils/coord-map-fixed)
        initial-lasers (get-initial-lasers coord-map)]
    (->> (iteration #(step coord-map %)
                    {:somef (comp seq :laser-tips)
                     :initk {:splits 0 :laser-tips initial-lasers}})
         last
         :splits)))

(part-1 test-input)

(part-1 (input/get-input 2025 7))

; ## Del 2

; I del 2 skal man finne alle mulige veier en laser kan gå. Dette blir nok en jobb for
; memoisering, slik at vi kan lagre og gjenbruke en del kalkulasjoner.

(def possible-paths
  (memoize
    (fn [coord-map laser-position]
      (let [[y x :as new-pos] (move-south laser-position)]
        (case (coord-map new-pos)
          \^ (+ (possible-paths coord-map [y (dec x)])
                (possible-paths coord-map [y (inc x)]))
          nil 1
          (recur coord-map new-pos))))))

; Det viste seg å være ganske effektivt, da hele problemet løses på under 100 ms.

(defn part-2 [input]
  (let [coord-map (->> input utils/text->matrix utils/coord-map-fixed)
        starting-position (first (get-initial-lasers coord-map))]
    (possible-paths coord-map starting-position)))

(part-2 test-input)

(part-2 (input/get-input 2025 7))