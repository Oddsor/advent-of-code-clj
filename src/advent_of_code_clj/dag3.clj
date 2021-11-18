(ns advent-of-code-clj.dag3
  (:require [clojure.string :as str]))

(def test-data "..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->")

(defn x-til-høyre [steg kart]
  (->> kart
       (map-indexed (fn [idx linje]
                      (let [høyreskift (mod (* steg idx) (count linje))
                            treff (get linje høyreskift)]
                        (= \# treff))))
       (filter true?)
       count))
(comment
  ;; Kartet er lengre enn det er bredt, så man må bruke modulo bredde for å få rett svar
  (->> (slurp "input/day3-input.txt")
       str/split-lines
       (x-til-høyre 3))
  )

(comment
  (let [kart (str/split-lines (slurp "input/day3-input.txt"))]
    (* (x-til-høyre 1 kart)
       (x-til-høyre 3 kart)
       (x-til-høyre 5 kart)
       (x-til-høyre 7 kart)
       (x-til-høyre 1 (take-nth 2 kart))))
  )