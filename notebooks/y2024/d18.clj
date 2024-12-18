(ns y2024.d18
  (:require
   [advent-of-code-clj.input :as input]
   [advent-of-code-clj.utils :as utils]
   [clojure.core.matrix :as mx]
   [ubergraph.alg :as ua]))

; # 2024, dag 18

; ## Del 1

; Test-inputten inneholder koordinater for noder som blokkerer en path fra [0 0] til [6 6].
; Her kan vi bruke `ubergraph` sin shortest-path-algoritme, men først må vi hente ut riktig
; antall blokkerende noder.

(def test-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

; I parsingen reverserer vi X og Y for å bedre passe inn i matrisen vår.

(defn corrupted-locations [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 2)
       (map reverse)
       (map vec)))

(corrupted-locations test-input)

; Pathen kan finnes ved å hente nabo-noder og filtrere vekk noder som er "out of bounds",
; og som er blokkert av `corrupted-locations`:

(defn find-path [[dimy dimx :as _dimensions] corrupted-locations]
  (let [in-bounds? (fn [[y x]]
                     (and (>= dimy y 0)
                          (>= dimx x 0)))]
    (ua/shortest-path
     (fn [node]
       (->> (apply utils/adjacent-hv node)
            (remove (some-fn corrupted-locations
                             (complement in-bounds?)))
            (map (fn [x] {:dest x}))))
     [0 0] [dimy dimx])))

; Viktig å huske på at vi ikke skal ta inn alle nodene i input, så funksjonen vår kan
; ta to argumenter for dimensjon og antall "bytes":

(defn part-1 [dimensions bytes input]
  (let [corrupted-locations (set (take bytes (corrupted-locations input)))]
    (:cost (find-path dimensions corrupted-locations))))

; Det gir oss følgende svar for del 1:

(delay (part-1 [6 6] 12 test-input))

(delay (part-1 [70 70] 1024 (input/get-input 2024 18)))

; For debugging har vi denne hjelpemetoden som printer ut matrisen:

(defn print-matrix [[y x :as dims] bytes input]
  (let [corrupted-nodes (set (take bytes (corrupted-locations input)))
        travel-path (set (ua/nodes-in-path (find-path dims corrupted-nodes)))]
    ^:kind/hiccup
    [:pre
     (with-out-str
       (mx/pm (mx/emap-indexed (fn [node _]
                                 (cond
                                   (corrupted-nodes node) \#
                                   (travel-path node) \O
                                   :else \.))
                               (mx/new-matrix (inc y) (inc x)))))]))

(delay (print-matrix [6 6] 12 test-input))

; ## Del 2

; I del 2 må vi finne ut hvilken node som vil føre til at det ikke er mulig å finne en
; gyldig path til målet.

; Dette løser vi ved å snevre inn søkeområdet basert på om vi finner en gyldig path på
; starten, midten og slutten av søkeområdet.

; (*TODO: denne algoritmen trenger litt refaktorering*)

(defn part-2 [dimensions input]
  (let [num-bytes (alength (.split input "\n"))
        corrupted-locations (corrupted-locations input)]
    (->> (nth corrupted-locations
              (loop [[start end] [0 num-bytes]]
                (if (>= 1 (- end start))
                  (first (filter (fn [x]
                                   (find-path dimensions (set (take x corrupted-locations))))
                                 [start end]))
                  (let [middle (int (+ (/ (- end start) 2) start))
                        corrupted-locations-start (set (take start corrupted-locations))
                        corrupted-locations-middle (set (take middle corrupted-locations))
                        corrupted-locations-end (set (take end corrupted-locations))
                        res-start (find-path dimensions corrupted-locations-start)
                        res-middle (find-path dimensions corrupted-locations-middle)
                        res-end (find-path dimensions corrupted-locations-end)]
                    (cond
                      (= nil res-end res-middle) (recur [start middle])
                      (and res-start res-middle) (recur [middle end]))))))
         reverse
         (map str)
         (String/join ","))))

(delay (part-2 [6 6] test-input))
(delay (part-2 [70 70] (input/get-input 2024 18)))
