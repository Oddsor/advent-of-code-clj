^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2024.d07
  (:require
   [criterium.core :as crit]))

; # 2024, dag 7

; ## Del 1

; Vi skal sjekke om en serie tall kan kombineres via addisjon/multiplikasjon
; slik at de gir et forhåndsbestemt resultat.

(def test-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

; For eksempel skal første linje gi riktig svar fordi 10 og 19 kan
; multipliseres til å bli 190, men line 3 vil aldri gi riktig svar
; fordi 17 ikke kan multipliseres eller adderes opp til 83

; En viktig regel er at operator-presedens ikke gjelder her, så man
; går alltid fra venstre mot høyre. Altså:

; `(81 * (40 + 27))`

; ikke

; `((81 * 40) + 27)`

; Dette kan løses med en rekursiv funksjon:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn solve-equation [solution operators acc [value & values]]
  (cond (> acc solution) false
        (nil? value) (= solution acc)
        :else (some identity
                    (map (fn [op]
                           (solve-equation solution operators (op acc value) values))
                         operators))))

; Og deretter filtrerer vi alle linjene basert på om de gir riktig
; svar eller ikke:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn solve [operators input]
  (let [lines (map #(map parse-long (re-seq #"\d+" %)) (.split input "\n"))]
    (->> lines
         (filter (fn [[solution val1 & remainder]]
                   (solve-equation solution operators val1 remainder)))
         (map first)
         (reduce +))))

(= 3749 (solve [+ *] test-input))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 1038838357795 (solve [+ *] (slurp "input/2024/input7.txt"))))

; ## Del 2

; Del 2 er ganske enkel denne gangen, siden vi bare behøver å legge til
; en ny type operator! 

; Originalt hadde ikke `solve`-funksjonen en liste med operatorer som
; parameter, men det la jeg til når jeg så at vi kunne gjenbruke resten
; av del 1 ved å legge til en ekstra parameter i en operator-liste:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn || [a b]
  (parse-long (str a b)))

(|| 12 34)

(= 11387 (solve [+ * ||] test-input))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 254136560217241 (solve [+ * ||] (slurp "input/2024/input7.txt"))))
