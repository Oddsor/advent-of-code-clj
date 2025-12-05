^:kindly/hide-code
(ns y2025.d03
  (:require
   [advent-of-code-clj.input :as input]
   [clojure.math :as math]
   [medley.core :as medley]))

; # 2025, dag 3

; Dårlig tid i dag, så det blir bare rett på brute-force løsning:

(def test-input "987654321111111
811111111111119
234234234234278
818181911112111")

(defn input->battery-banks [input]
  (->> (String/.split input "\n")
       (map (fn [line]
              (mapv (comp Integer/parseInt str) line)))))

(input->battery-banks test-input)

(defn all-number-combinations [bank]
  (let [max-n (count bank)]
    (for [a (range 0 (dec max-n))
          b (range (inc a) max-n)]
      (Integer/parseInt (str (bank a) (bank b))))))

(take 20 (all-number-combinations [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1]))

(defn jolt [bank]
  (apply max (all-number-combinations bank)))

(jolt [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1])

(defn part-1 [input]
  (->> input
       input->battery-banks
       (map jolt)
       (reduce +)))

(part-1 test-input)

(part-1 (input/get-input 2025 3))

; ## Del 2

; I del 2 skal man se på alle siffrene på hver linje i test-inputten,
; og skal finne det største mulige tallet bestående av 11 siffer.

; I utgangspunktet høres dette ut som en dynamisk programmeringsoppgave
; som vil kreve memoisering og lignende, men det skal være nok å bare
; lete etter største mulige siffer som fortsatt vil gi et tall bestående av
; 11 siffer. Feks for 234234234234278 så er det høyeste første sifferet 4:

(defn find-largest-next-idx-and-digit [acc remaining]
  (let [digits-so-far (count acc)
        remaining-digits (count remaining)
        droppable-digits (- remaining-digits (- 11 digits-so-far))
        relevant-digits (take droppable-digits remaining)
        first-largest-number (fn [[idx1 v1] [idx2 v2]]
                               (let [c1 (compare v2 v1)
                                     c2 (compare idx1 idx2)]
                                 (if (zero? c1)
                                   c2 c1)))]
    (->> relevant-digits
         medley/indexed
         (sort first-largest-number)
         first)))

(find-largest-next-idx-and-digit [] (map (comp parse-long str) "234234234234278"))
(find-largest-next-idx-and-digit [] (map (comp parse-long str) "818181911112111"))

; Nå kan vi loope igjennom siffrene og hente ut det høyest mulige sifferet helt til 
; vi har bygd opp et tall bestående av 12 siffer:

(defn build-largest-number [bank]
  (loop [current-digits []
         remaining-digits bank]
    (if (>= (count current-digits) 12)
      current-digits
      (let [[idx digit] (find-largest-next-idx-and-digit current-digits remaining-digits)]
        (recur (conj current-digits digit)
               (drop (inc idx) remaining-digits))))))

(build-largest-number (map (comp parse-long str) "234234234234278"))
(build-largest-number (map (comp parse-long str) "818181911112111"))

; Vi kunne satt sammen listen av siffer til tall ved å joine listen til en string,
; og deretter parse til tall, men vi kan også forsøke å lage en mer lavnivå funksjon
; som har høyere ytelse:

(defn assemble-number [digit-vector]
  (let [digit-length (count digit-vector)]
    (loop [scalar (dec digit-length)
           idx 0
           acc 0]
      (if (> digit-length idx)
        (let [digit (digit-vector idx)
              scale (long (math/pow 10 scalar))]
          (recur
            (dec scalar)
            (inc idx)
            (+ acc ^long (* digit scale))))
        acc))))

; Vi har nå alt vi trenger for å løse del 2:

(defn part-2 [input]
  (->> input 
       input->battery-banks
       (mapv build-largest-number)
       (mapv assemble-number)
       (reduce +)))

(= 3121910778619 (part-2 test-input))

(part-2 (input/get-input 2025 3))