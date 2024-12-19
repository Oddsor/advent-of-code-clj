^:kindly/hide-code
(ns y2024.d19
  (:require
   [advent-of-code-clj.input :as input]))

; 2024, dag 19

; ## Del 1

; Gitt en rekke patterns skal vi finne ut om en håndduk (towel)
; kan bygges opp av bestanddelene:

(def test-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

; Starter med å skille patterns og håndduker:

(defn parse [input]
  (let [[patterns towels] (.split input "\n\n")]
    [(re-seq #"\w+" patterns)
     (.split towels "\n")]))

(parse test-input)

; En håndduk er gyldig dersom starten på duken matcher et av
; mønstrene, og resten av duken matcher et mønster. Altså en rekursiv
; algritme:

(defn valid-pattern? [patterns ^String towel]
  (if (= "" towel)
    true
    (some
     (fn [p]
       (when (.startsWith towel p)
         (valid-pattern? patterns (subs towel (count p) (count towel)))))
     patterns)))

; Fra oppgaveteksten vet vi at vi kan forvente at duken `brwrr` er gyldig,
; men `ubwu` ikke er gyldig. Vi tester funksjonen vår med disse:

(def test-patterns (first (parse test-input)))

(valid-pattern? test-patterns "brwrr")
(valid-pattern? test-patterns "ubwu")

; Når vi ser at algoritmen fungerer kan vi telle antall gyldige duker og løse del 1:

(defn part-1 [input]
  (let [[patterns towels] (parse input)]
    (count (keep #(valid-pattern? patterns %)
                 towels))))

(part-1 test-input)

; *TODO: Algoritmen er for treg til å kjøres når notebooken bygger, så vi
; må wrappe den i en kommentar (tar 2.3 sekunder og gir svaret `296`):*

(comment
  (time
   (part-1 (input/get-input 2024 19))))

; ## Del 2

; Denne gangen skal vi ikke bare sjekke om en duk er gyldig, men telle alle
; kombinasjoner av mønstre som utgjør en gyldig duk. Fremgangsmåten er ganske
; lik, men vi avslutter med å returnere et heltall for hver gyldige duk,
; og summere opp alle "branches" som vi fant på veien.

; Memoisering tar ned kjøretiden fra rundt 76 sekunder til 8.4 på reell input,
; som fortsatt er ganske tregt:

(def valid-patterns
  (memoize
   (fn [patterns ^String towel]
     (if (= "" towel)
       1
       (transduce (map (fn [p]
                         (if (.startsWith towel p)
                           (valid-patterns patterns (subs towel (count p) (count towel)))
                           0)))
                  + patterns)))))

; Nå kan vi summere opp alle kombinasjoner og løse del 2:

(defn part-2 [input]
  (let [[patterns towels] (parse input)]
    (transduce (map #(valid-patterns patterns %))
               + towels)))

(part-2 test-input)

; *TODO: del 2 kjører også for tregt og må wrappes i en kommentarblokk.
; Tar 16 sekunder å kjøre, og gir svaret `619970556776002`.*
(comment
  (time
   (part-2 (input/get-input 2024 19))))

; ## Del 1 og 2 med typehints

; Det viser seg at algoritmene egentlig ikke er så trege! Algoritmen bruker mye tid på reflection fordi
; vi kaller `.startsWith` på en variabel som kompilatoren ikke kan garantere at alltid er en `String`.

; Se [dokumentasjon om type hinting](https://clojure.org/reference/java_interop#typehints)

; Når kjører koden med reflection-warning finner vi de to stedene som er særlig utsatt fordi de er i en
; "hot loop":

^:kind/hiccup
[:pre "(set! *warn-on-reflection* true)"]

^:kind/hiccup
[:pre "Reflection warning,... - call to method startsWith can't be resolved (target class is unknown)."]

; Hvis vi lager nye type-hintede versjoner av `valid-pattern`-algoritmene kuttes kjøretiden ned til et mer
; akseptabelt nivå (~80ms og ~300ms):

(defn valid-pattern?-hinted [patterns ^String towel]
  (if (= "" towel)
    true
    (some
     (fn [p]
       (when (.startsWith towel p)
         (valid-pattern?-hinted patterns (subs towel (count p) (count towel)))))
     patterns)))

(def valid-patterns-hinted
  (memoize
   (fn [patterns ^String towel]
     (if (= "" towel)
       1
       (transduce (map (fn [p]
                         (if (.startsWith towel p)
                           (valid-patterns-hinted patterns (subs towel (count p) (count towel)))
                           0)))
                  + patterns)))))

(defn part-1-hinted [input]
  (let [[patterns towels] (parse input)]
    (count (keep #(valid-pattern?-hinted patterns %)
                 towels))))

(delay (part-1-hinted (input/get-input 2024 19)))

(defn part-2-hinted [input]
  (let [[patterns towels] (parse input)]
    (transduce (map #(valid-patterns-hinted patterns %))
               + towels)))

(delay (part-2-hinted (input/get-input 2024 19)))
