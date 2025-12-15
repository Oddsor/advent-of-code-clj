^:kindly/hide-code
(ns y2025.d02
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.core.reducers :as r]))

; # 2025, dag 2

; ## Del 1

; I dag skal vi kverne igjennom en serie med tall, og finne ugyldige ID'er innenfor de gitte områdene.

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

; For å finne alle mulige tall i inputten kan vi konkatenere intervallene til en diger liste som vi kan behandle.

(defn get-numeric-sequence [input-text]
  (->> input-text
       (re-seq #"(\d+)-(\d+)")
       (mapcat (fn [[_ range-start range-end-inclusive]]
                 (let [r-start (parse-long range-start)
                       r-end (parse-long range-end-inclusive)]
                   (range r-start (inc r-end)))))))

(def test-sequence (get-numeric-sequence test-input))

(take 10 test-sequence)

; Hva definerer et ugyldig siffer? Det består av to repeterende nummer, som feks 11 (1,1), 123123 (123, 123),
; osv. Dette kan vi løse naivt ved å konvertere samtlige siffer til string, splitte på midten, og sjekke om de
; to delene er like:

(defn two-repetitions? [number]
  (let [numeric-string (str number)
        str-len (count numeric-string)
        left-side (subs numeric-string 0 (/ str-len 2))
        right-side (subs numeric-string (/ str-len 2) str-len)]
    (= left-side right-side)))

; Eller ved regex hvor vi fanger opp et mønster, og sier at det skal følges 
; av samme mønster en gang til:

(defn two-repetitions-re? [number]
  (re-matches #"([0-9]+)\1" (str number)))

(two-repetitions? 11)
(two-repetitions-re? 11)
(two-repetitions? 12)
(two-repetitions-re? 12)
(two-repetitions? 123123)
(two-repetitions-re? 123123)
(two-repetitions? 123122)
(two-repetitions-re? 123122)

; Da er det bare å summere ugyldige tall basert på et predikat. Vi kan kaste litt flere tråder
; på problemet ved å bruke `fold` fra reducers-namespacet (men kun dersom vi konverterer listen
; med siffer til en vektor):

(defn sum-all-invalid-numbers [predicate-fn numeric-sequence]
  (r/fold + + (r/filter predicate-fn (vec numeric-sequence))))

(= (sum-all-invalid-numbers two-repetitions? test-sequence) 1227775554)

(delay
  (sum-all-invalid-numbers two-repetitions-re? (get-numeric-sequence (input/get-input 2025 2))))

; ## Del 2

; I skal vi finne mønster som repeterer seg opptil flere ganger, altså i stedet for at bare
; "venstre" og "høyre" side av sifferet er like kan det nå være feks kombinasjoner som 
; repeteres minst tre ganger: 121212 (12, 12, 12), 111 (1, 1, 1) og så videre.

; Det enkleste her er sannsynligvis å lage større og større partisjoner av sifferet i tekst-form
; helt opptil halve sifferets lengde og sjekke om alle partisjonene er like:

(defn repeating-sequence? [number]
  (let [numeric-string (str number)
        str-len (count numeric-string)
        max-partition (/ str-len 2)]
    (loop [partition-size 1]
      (if (> partition-size max-partition) false
          (let [search-string (subs numeric-string 0 partition-size)
                pattern (re-pattern (str "^(" search-string ")+$"))]
            (if (re-matches pattern numeric-string)
              true
              (recur (inc partition-size))))))))

; Eller vi kan bruke samme regex-triks som over, men denne gangen sier vi at
; mønsteret kan repetere en eller flere ganger

(defn repeating-sequence-re? [number]
  (let [numeric-string (str number)]
    (re-matches #"^([0-9]+)\1+$" numeric-string)))

(repeating-sequence? 12341234)
(repeating-sequence? 123123123)
(repeating-sequence? 111111111111)
(repeating-sequence? 1188511885)
(repeating-sequence? 2121212121)
(repeating-sequence-re? 12341234)
(repeating-sequence-re? 123123123)
(repeating-sequence-re? 111111111111)
(repeating-sequence-re? 1188511885)
(repeating-sequence-re? 2121212121)

(filter repeating-sequence? test-sequence)

; En litt tregere komputasjon, men løser iallfall oppgaven:

(= 4174379265 (sum-all-invalid-numbers repeating-sequence? test-sequence))

; Resultat for del 2:

(sum-all-invalid-numbers repeating-sequence-re?
                         (get-numeric-sequence (input/get-input 2025 2)))
