^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2024.d07
  (:require
    [nextjournal.clerk :as clerk]
    [advent-of-code-clj.input :as input]))

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
(defn solve-equation
  ([solution operators [value & values]] (solve-equation solution operators value values))
  ([solution operators acc [value & values]]
   (cond (> acc solution) false
         (nil? value) (= solution acc)
         :else (some #(solve-equation solution operators (% acc value) values)
                     operators))))

(clerk/example
  (solve-equation 3267 [+ *] [81 40 27]))

; Og deretter filtrerer vi alle linjene basert på om de gir riktig
; svar eller ikke:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn solve [operators input]
  (let [lines (map #(map parse-long (re-seq #"\d+" %)) (String/.split input "\n"))]
    (->> lines
         (filter (fn [[solution & values]]
                   (solve-equation solution operators values)))
         (map first)
         (reduce +))))

; Dermed kan vi løse del 1 for test-input:

(= 3749 (solve [+ *] test-input))

; Som gir oss svaret på reell input:

(= 1038838357795 (solve [+ *] (input/get-input 2024 7)))

; ## Del 2

; Del 2 er ganske enkel denne gangen, siden vi bare behøver å legge til
; en ny type operator! 

; Originalt hadde ikke `solve`-funksjonen en liste med operatorer som
; parameter, men det la jeg til når jeg så at vi kunne gjenbruke resten
; av del 1 ved å legge til en ekstra parameter i en operator-liste:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn || [a b]
  (parse-long (str a b)))

(clerk/example
  (|| 12 34))

; Med den nye operatoren kan feks denne linjen bli sann: `7290: 6 8 6 15`

(clerk/example
  (solve-equation 7290 [+ *] [6 8 6 15])
  (solve-equation 7290 [+ * ||] [6 8 6 15]))

; Den nye operatoren gir svaret for del 2 på test-input:

(= 11387 (solve [+ * ||] test-input))

; Og riktig svar på reell input:

(= 254136560217241 (solve [+ * ||] (input/get-input 2024 7)))
