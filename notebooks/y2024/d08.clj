(ns y2024.d08
  (:require
   [advent-of-code-clj.utils :as utils]
   [clojure.core.matrix :as mx]
   [medley.core :as medley]
   [nextjournal.clerk :as clerk]
   [advent-of-code-clj.input :as input]))

; 2024, dag 8

; Gitt et kart over antenner, skal vi finne alle "antinoder". En antinode
; er enkelt forklart en posisjon "på den andre siden" av en antenne relativt
; til en annen antenne av samme "type" (A og 0 i test-dataene er to typer antenner).

(def test-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

; ## Del 1

; Funksjonen for å finne antinoden til en antenne blir som følger:

(defn antinodes-1 [[dimy dimx] antenna2 antenna1]
  (filter (fn [[antinode-x antinode-y]]
            (and (< -1 antinode-x dimx)
                 (< -1 antinode-y dimy)))
          [(mx/add antenna2
                   (mx/sub antenna2 antenna1))]))

(clerk/example
 (first (antinodes-1 [12 12] [9 9] [8 8]))
 (first (antinodes-1 [12 12] [5 5] [10 10])))

; Dermed kan vi finne alle antinoder ved å kjøre funksjonen på alle
; antenner i en gruppe:

(defn find-antinodes [antinode-fn dims antenna-coords]
  (->> (for [antenna1 antenna-coords
             antenna2 antenna-coords
             :when (not= antenna1 antenna2)
             :let [antinodes (antinode-fn dims antenna1 antenna2)]]
         antinodes)
       (mapcat identity)))

(clerk/example
 (find-antinodes antinodes-1 [12 12] [[6 5] [9 9] [8 8]]))

; Og vi trenger litt oppsett for å gruppere antennetyper og samle opp
; alle distinkte antinoder:

(defn solve [antinode-fn matrix]
  (let [dims (mx/shape matrix)
        coord-map (utils/coord-map-fixed matrix)
        grouped (dissoc (medley/collate-by val merge #(apply assoc {} %) coord-map)
                        \.)]
    (->> (for [[_ antenna-coords] grouped]
           (find-antinodes antinode-fn dims (keys antenna-coords)))
         (mapcat identity)
         distinct)))

; Som gir oss riktig svar for test-input:

(= 14 (count (solve antinodes-1 (utils/text->matrix test-input))))

; og reell input:
(comment
  (= 381 (count (solve antinodes-1 (utils/text->matrix (input/get-input 2024 8))))))

; ## Del 2

; I del 2 må vi hente et repeterende mønster med antinoder. Altså gjør vi som i
; del 1, men repeterer langs aksen helt til vi er "out of bounds":

(defn antinodes-2 [[dimy dimx] antenna2 antenna1]
  (let [increment (mx/sub antenna2 antenna1)]
    (->> (iterate #(mx/add % increment) antenna2)
         (take-while (fn [[nx ny]]
                       (and (< -1 nx dimx)
                            (< -1 ny dimy)))))))

; Eksempel fra oppgaven med "T-master":
(reduce (fn [mx [x y]] (mx/mset mx x y \T)) (mx/broadcast \. [10 10]) [[0 0] [2 1] [1 3]])

; Hvor antinodene da ender opp med å være:

(let [mx (reduce (fn [mx [x y]] (mx/mset mx x y \T)) (mx/broadcast \. [10 10]) [[0 0] [2 1] [1 3]])
      antinodes (solve antinodes-2 mx)]
  (reduce (fn [mx [x y]] (mx/mset mx x y \#)) mx antinodes))

; Nå finner vi riktig svar i test-input:

(= 34 (count (solve antinodes-2 (utils/text->matrix test-input))))

; Og riktig svar i reell input:

(= 1184 (count (solve antinodes-2 (utils/text->matrix (input/get-input 2024 8)))))
