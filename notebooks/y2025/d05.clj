^:kindly/hide-code
(ns y2025.d05
  (:require
    [advent-of-code-clj.input :as input]))

; # 2025, dag 5

; ## Del 1

; I dag skal vi se igjennom en liste med tallområder og sjekke om en annen liste med
; tall er innenfor minst ett område. Et tall korresponderer til en id for en "ingrediens".

(def test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

; Parse-funksjonen splitter opp inputten i to. Den første delen blir til en serie med vektorer bestående av
; min og maks-verdi for et område, og den andre delen er en liste med tall.

(defn parse-input [input]
  (let [[fresh-range-str ingredient-id-str] (String/.split input "\n\n")
        fresh-ranges (->> fresh-range-str
                          (re-seq #"(\d+)-(\d+)")
                          (map (fn [[_ min-inc max-inc]]
                                 [(parse-long min-inc)
                                  (parse-long max-inc)])))
        ingredient-ids (map parse-long (String/.split ingredient-id-str "\n"))]
    {:fresh-ranges fresh-ranges
     :ingredients ingredient-ids}))

(parse-input test-input)

; En ingrediens er "fersk" dersom den er innenfor et av tallområdene:

(defn within-range? [ranges ingredient]
  (some (fn [[rmin rmax]]
          (>= rmax ingredient rmin)) ranges))

; Del 1 handler om å finne ut hvor mange ingredienser som er ferske, altså
; hvor mange tall som er innenfor et tallområde:

(defn part-1 [input]
  (let [{:keys [fresh-ranges ingredients]} (parse-input input)]
    (count (filter #(within-range? fresh-ranges %) ingredients))))

(part-1 test-input)

(part-1 (input/get-input 2025 5))

; ## Del 2

; I del to skal vi finne alle mulige tall som omfattes av tallrekkene i del 1.
; Her vil nok en brute-force-metode neppe fungere ettersom ekte input opererer med
; veldig store tall.

; Løsningen blir kanskje å loope igjennom listen over ranges og se om vi kan redusere
; overlappene.

(defn overlapping-range [range-1 range-2]
  (let [[r-min-1 r-max-1] range-1
        [r-min-2 r-max-2] range-2]
    (when (or (<= r-min-1 r-min-2 r-max-1)
              (<= r-min-1 r-max-2 r-max-1))
      [(min r-min-1 r-min-2)
       (max r-max-1 r-max-2)])))

(overlapping-range [10 14] [12 18])
(overlapping-range [12 18] [10 14])
(overlapping-range [16 20] [10 14])
(overlapping-range [10 14] [16 20])

; Denne litt stygge algoritmen looper igjennom listen av tallområder, prøver å finne en
; som overlapper med en annen, og slår de sammen. Når det ikke er flere overlappende
; områder går vi ut av loopen.

(defn reduce-ranges [ranges]
  (loop [ranges-to-reduce (distinct ranges)]
    (if-let [[r1 r2 overlapping-range] (first (for [r1 ranges-to-reduce
                                                    r2 ranges-to-reduce
                                                    :when (not= r1 r2)
                                                    :let [overlapping (overlapping-range r1 r2)]
                                                    :when overlapping]
                                                [r1 r2 overlapping]))]
      (recur (cons overlapping-range (remove #{r1 r2} ranges-to-reduce)))
      ranges-to-reduce)))

; Nå kan vi lage en metode som får en haug tallområder, slår sammen om mulig, og deretter
; teller opp hvor mange tall som er innenfor de ulike områdene:

(defn number-of-fresh-ids [ranges]
  (let [reduced-ranges (reduce-ranges ranges)]
    (reduce (fn [acc [rmin rmax]]
              (+ acc (inc (- rmax rmin)))) 0 reduced-ranges)))

; Og da har vi løst del 2:

(->> (parse-input test-input)
     :fresh-ranges
     number-of-fresh-ids
     (= 14))

(->> (parse-input (input/get-input 2025 5))
     :fresh-ranges
     number-of-fresh-ids
     (= 354226555270043))