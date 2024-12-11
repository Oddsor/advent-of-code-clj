(ns y2024.d11
  (:require
   [advent-of-code-clj.input :as input]))

; # 2024, dag 11

; ## Del 1 og 2

; Vi skal finne ut hvor mange "steiner" vi sitter igjen med etter å ha gått igjennom et par
; iterasjoner hvor vi dobler steinene basert på noen enkle regler:

; 1. Steiner med tallet 0 blir til steiner med tallet 1.
; 2. Steiner med et partall antall siffer blir delt i 2 (feks 1234 blir til 12 og 34)
; 3. Resten multipliseres med 2024

(defn split-number [number]
  (let [nstr (str number)
        left-half (.substring nstr 0 (/ (.length nstr) 2))
        right-half (.substring nstr (/ (.length nstr) 2) (.length nstr))]
    [(parse-long left-half) (parse-long right-half)]))

(split-number 123456)

; Gitt disse reglene skal vi se hvor mange steiner vi har etter 25 iterasjoner.
; En naiv løsning med en økende liste med tall vil neppe fungere for del 2, da vi garantert
; skal øke antall iterasjoner, så her må vi memoisere.

(def test-input "125 17")

(defn digits [number] (count (str number)))

(digits 13456)

; Her er den memoiserte funksjonen:

(def blink
  (memoize
   (fn [n number]
     (if (zero? n)
       1
       (cond
         (zero? number) (blink (dec n) 1)
         (-> number digits even?) (let [[n1 n2] (split-number number)]
                                    (+ (blink (dec n) n1)
                                       (blink (dec n) n2)))
         :else (blink (dec n) (* 2024 number)))))))

; Vi looper igjennom de "steinene" vi har fra test-input og reell input i en solve-funksjon:

(defn solve [iterations input]
  (transduce
   (comp (map parse-long)
         (map #(blink iterations %)))
   +  (re-seq #"\d+" input)))

; Med det får vi riktig svar på all input for del 1:

; Elapsed time: 56.157468 msecs
(solve 25 test-input)

(solve 25 (input/get-input 2024 11))

; Og del 2:

; Elapsed time: 220.008672 msecs
(solve 75 (input/get-input 2024 11))

; ## Med frequencies

; I stedet for rekursiv memoisering kan vi bruke en hashmap for å "deduplisere"
; like tall underveis, og holde telling på antallet like tall.

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn solve-iteration [freqs]
  (reduce-kv (fn [acc ^long k v]
               (let [update-add (fn [c] (+ (or c 0) v))]
                 (cond
                   (zero? k) (update acc 1 update-add)
                   (-> k digits even?) (let [[n1 n2] (split-number k)]
                                         (-> acc
                                             (update n1 update-add)
                                             (update n2 update-add)))
                   :else (update acc (* 2024 k) update-add))))
             {} freqs))

(defn solve-freq [n input]
  (let [numbers (map parse-long (re-seq #"\d+" input))
        num-freqs (frequencies numbers)]
    (->> (nth (iterate solve-iteration num-freqs) n)
         vals (reduce +))))

(solve-freq 25 test-input)

(solve-freq 25 (input/get-input 2024 11))

; Elapsed time: 116.157357 msecs
(solve-freq 75 (input/get-input 2024 11))

(defn solve-loopy [^long n input]
  (loop [n n
         acc (frequencies (map parse-long (re-seq #"\d+" input)))]
    (if (zero? n)
      (->> acc vals (reduce +))
      (recur (dec n)
             (solve-iteration acc)))))

; Elapsed time: 114.77593 msecs
(solve-loopy 75 (input/get-input 2024 11))
