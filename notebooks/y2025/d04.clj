^:kindly/hide-code
(ns y2025.d04
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.utils :as utils]
    [clojure.core.matrix :as mx]
    [medley.core :as medley]))

; # 2025, dag 4

; ## Del 1

; Vi skal se på en grid, som representerer en avdeling full
; av papirruller. Oppgaven blir å finne tilgjengelige papirruller,
; definert ved at det er nok "luft" rundt papirrullen.

; Med andre ord skal vi finne alle papirruller som er omgitt av
; færre enn 4 andre papirruller.

(def test-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

; Vi børster støvet av gode gamle util-metoder for å opprette
; matriser og omforme de til en hashmap med koordinater:

(def test-matrix
  (utils/text->matrix test-input))

test-matrix

(def test-map (utils/coord-map-fixed test-matrix))

; En papirrull er tilgjengelig dersom den er omgitt av
; færre enn 4 andre papirruller:

(defn accessible-paper-roll? [coord-map node]
  (let [adjacent-nodes (apply utils/adjacent node)]
    (->> (for [an adjacent-nodes
               :when (#{\@} (coord-map an))]
           an)
         count
         (> 4))))

(accessible-paper-roll? test-map [1 0])

; Med denne predikatfunksjonen kan vi hente ut et sett av tilgjengelige
; papirruller:

(defn find-accessible-rolls [coord-map]
  (->> coord-map
       (medley/filter-vals #{\@})
       keys
       (filter #(accessible-paper-roll? coord-map %))
       set))

(def test-accessible-rolls (find-accessible-rolls test-map))

; Vi oppretter en funksjon for å visualisere hvilke papirruller
; som er tilgjengelige:

(defn print-matrix [[y x :as _dims] accessible input-map]
  ^:kind/hiccup
  [:pre
   (with-out-str
     (mx/pm (mx/emap-indexed (fn [node _]
                               (cond (accessible node) \X
                                     :else (let [x (input-map node)]
                                             (if (= x \.) \space x))))
                             (mx/new-matrix y x))))])

(print-matrix [(count (first test-matrix)) (count test-matrix)]
              test-accessible-rolls
              test-map)

; Utskriften over er tilsynelatende identisk med den vi finner
; i oppgaveteksten, så det ser ut til at vi er i mål.

; Løsningen på del 1 er enkelt og greit antallet tilgjengelige
; papirruller, som vi allerede har funnet.

; Så om vi gjentar stegene og teller opp tilgjengelige ruller har vi svaret
; for del 1:

(defn part-1 [input]
  (->> input
       utils/text->matrix
       utils/coord-map-fixed
       find-accessible-rolls
       count))

(part-1 test-input)

(part-1 (input/get-input 2025 4))

; ## Del 2

; I del 2 kan man gjenta prosessen med å finne tilgjengelige papirruller, fjerne de, og
; finne nye tilgjengelige papirruller.

; Det løser vi ved å loope igjennom koordinat-kartet, telle opp ruller som kan fjernes,
; fjerne rullene og starte på nytt, helt til vi ikke lenger kan fjerne ruller.

(defn part-2 [input]
  (loop [coord-map (->> input utils/text->matrix utils/coord-map-fixed)
         removed-rolls #{}]
    (if-let [accessible-rolls (not-empty (find-accessible-rolls coord-map))]
      (recur (medley/remove-keys accessible-rolls coord-map)
             (into removed-rolls accessible-rolls))
      (count removed-rolls))))

(delay (part-2 test-input))

(delay (part-2 (input/get-input 2025 4)))