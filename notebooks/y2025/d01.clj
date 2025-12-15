^:kindly/hide-code
(ns y2025.d01
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.string :as str]))

; # 2025, dag 1

; ## Del 1

; Første oppgave i år er ganske grei; gitt et startpunkt og en serie med addisjoner/subtraksjoner,
; finn hvor mange ganger man treffer tallet null.

; Startposisjon er 50, og minimum og maksimumsverdiene er 0 og 99. Bruker modulo for å holde verdiene
; innenfor dette området.

; Feks skal man få resultatet 3 med følgende testdata:

(def test-data "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

; Første tanke på løsning som ikke er helt optimal, men ganske elegant, er å bruke `reductions`, som lar oss
; definere en initiell verdi, redusere over sekvensen, og få en sekvens av alle mellomverdier som resultat.

; Først trenger vi å omforme teksten til en sekvens av tall:

(defn parse-input [input-text]
  (->> (str/replace input-text #"[LR]" {"L" "-"
                                        "R" ""})
       str/split-lines
       (mapv Integer/parseInt)))

(def test-increments
  (parse-input test-data))

(defn new-position ^long [init increment]
  (mod (+ init increment) 100))

(defn positions [increment]
  (reductions new-position
              50
              increment))

(positions test-increments)

; Så kan vi redusere over sekvensen og telle antall treff på null:

(defn part-1 [increments]
  (->> (positions increments)
       (filter zero?)
       count))

(part-1 test-increments)

; Som forventet ble svaret 3, så da kan vi prøve med det faktiske inputet:

(part-1 (parse-input (input/get-input 2025 1)))

; ## Del 2

; Del 2 har en vrien vri; nå skal vi ta høyde for alle ganger vi "treffer null", ikke bare gangene
; vi lander på null. For eksempel vil første bevegelse (L68) treffe 0 og lande på 82.
; I tillegg vil feks bevegelsen "R1000" treffe null 10 ganger, som vi også må ta høyde for.

; Forsøkte initielt å løse denne matematisk, men endte opp med en "bruteforce" variant fordi jeg fikk
; feil svar på ekte input.

(defn apply-movement [start increment]
  (let [dir-fn (if (neg? increment) dec inc)
        dec-fn (if (neg? increment) inc dec)]
    (loop [current start
           num-zeroes 0
           remaining increment]
      (if (zero? remaining)
        [current num-zeroes]
        (let [new-pos (mod (dir-fn current) 100)]
          (recur new-pos
                 (if (zero? new-pos)
                   (inc num-zeroes)
                   num-zeroes)
                 (dec-fn remaining)))))))

(apply-movement 50 1000)
(apply-movement 50 -68)

(defn part-2 [increments]
  (loop [pos 50
         [increment & remainder] increments
         num-zeroes 0]
    (if increment
      (let [[new-pos hits] (apply-movement pos increment)]
        (recur (long new-pos)
               remainder
               (+ num-zeroes ^long hits)))
      num-zeroes)))

(part-2 test-increments)

(part-2 (parse-input (input/get-input 2025 1)))
