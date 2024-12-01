^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns y2024.d01)

; # Y2024, D01

; Denne oppgaven er relativt enkel, men ser ut til å være
; litt uvanlig i parsingen ettersom man har to lister som
; går vertikalt i inputten:

^{:nextjournal.clerk/visibility {:result :hide}}
(def test-data "3   4
4   3
2   5
1   3
3   9
3   3")

; ## Del 1

; Vi skal finne distansene mellom verdiene i hver liste,
; sortert fra lavest til høyest.

; Vi kan starte med å få tak i listene:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-lists [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)
       (partition 2)
       (apply mapv vector)))

(get-lists test-data)

; Når vi har listene kan vi sortere og finne differansen
; mellom hvert siffer, og summere:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn difference [input]
  (->> (get-lists input)
       (map sort)
       (apply map (comp abs -))
       (reduce +)))

(= 11 (difference test-data))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 2756096 (difference (slurp "input/2024/input1.txt"))))

; ## Del 2

; Denne gangen finner vi antall ganger et siffer i den første
; listen dukker opp i den andre:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn total-similarity-score [input]
  (let [[list-a list-b] (get-lists input)
        freqs (frequencies list-b)
        similarity-score (fn [x] (* x (freqs x 0)))]
    (transduce (map similarity-score)
               +
               list-a)))

(= 31 (total-similarity-score test-data))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 23117829 (total-similarity-score (slurp "input/2024/input1.txt"))))
