^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d02
  (:require [clojure.string :as str]))

;; # Year 2023, day 2

;; ## Part 1

(def test-data "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def max-counts {:blue 14 :green 13 :red 12})

^{:nextjournal.clerk/visibility {:result :hide}}
(defn line->game [line]
  (reduce
   #(assoc %1 %2 (->> line (re-seq (re-pattern (str "(\\d+) " (name %2)))) (mapv (comp parse-long second))))
   {:idx (->> line (re-find #"\d+") parse-long)}
   (keys max-counts)))

(->> test-data str/split-lines (map line->game))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn max-for-each-color
  "Find the maximum amount of cubes pulled for a given game."
  [game]
  (reduce #(update %1 %2 (partial apply max)) game (keys max-counts)))

(->> test-data str/split-lines (map line->game) (map max-for-each-color))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1
  "Only count the game number for games that are valid.
   Games are valid if the number of cubes pulled at once
   of a given color does not exceed the maximum amount
   given by max-counts"
  [games]
  (->> games
       (map max-for-each-color)
       (filter #(every? (fn [[color count]] (>= count (% color))) max-counts))
       (map :idx)
       (apply +)))

(= 8 (->> test-data str/split-lines (map line->game) part-1))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 2541 (->> (slurp "input/2023/d02.txt") str/split-lines (map line->game) part-1)))

;; ## Part 2

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2
  "Multiply the minimum required amount of cubes of every color
   required for each game then find the sum of all games."
  [games]
  (->> games
       (map max-for-each-color)
       (map #(->> (select-keys % (keys max-counts))
                  vals
                  (apply *)))
       (apply +)))

(= 2286 (->> test-data str/split-lines (map line->game) part-2))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 66016 (->> (slurp "input/2023/d02.txt") str/split-lines (map line->game) part-2)))