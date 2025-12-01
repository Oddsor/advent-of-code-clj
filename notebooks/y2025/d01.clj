^:kindly/hide-code
(ns y2025.d01
  (:require
    [advent-of-code-clj.input :as input]))

; # 2025, dag 1

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

(defn line->adjustment [line]
  (let [[dir n] [(first line) (Integer/parseInt (subs line 1))]]
    (case dir
      \L (- n)
      \R n)))

(defn parse-input [input-text]
  (map line->adjustment (String/.split input-text "\n")))

(parse-input test-data)

; Så kan vi redusere over sekvensen og telle antall treff på null:

(defn part-1 [adjustments]
  (let [start 50
        values (reductions (fn [acc x]
                             (mod (+ acc x) 100))
                           start
                           adjustments)]
    (count (filter zero? values))))

(part-1 (parse-input test-data))

; Som forventet ble svaret 3, så da kan vi prøve med det faktiske inputet:

(part-1 (parse-input (input/get-input 2025 1)))
