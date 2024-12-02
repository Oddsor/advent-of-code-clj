(ns y2024.d02)

; # 2024, dag 2

; ## Del 1

; Relativt grei oppgave. Vet ikke om

(def test-data "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

^{:nextjournal.clerk/visibility {:result :hide}}
(defn row-safe?
  "Hvis alle er positive eller negative,
   og hvis endringen er mellom 1 og 3, er alt bra!"
  [row]
  (let [differences (map - row (rest row))]
    (and (or (every? neg-int? differences)
             (every? pos-int? differences))
         (every? (fn [x] (>= 3 (abs x) 1)) differences))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn safe-lines [input]
  (->> (.split input "\\n")
       (map (fn [line] (map parse-long (.split line "\\s+"))))
       (filter row-safe?)
       count))

(safe-lines test-data)

(comment
  (= 314 (safe-lines (slurp "input/2024/input2.txt"))))

; ## Del 2

; Nå tillater vi én feil per rad. Løsningen fra del 1 kan
; gjenbrukes i en brute-force løsning.

; Først må vi kunne lage en sekvens av rader som tilsvarer
; en rad, uten ett av elementene:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn without-one-element [row]
  (for [i (range (count row))]
    (concat (take i row) (drop (inc i) row))))

(without-one-element [1 2 3 4 5 6])

; Så filtrerer vi ut de radene som allerede er trygge,
; og kjører de utrygge på nytt minus ett element:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn safeish-lines [input]
  (let [rows (->> (.split input "\\n")
                  (map (fn [line] (map parse-long (.split line "\\s+")))))
        {safe true unsafe false} (group-by row-safe? rows)
        safe-unsafe-rows (filter (fn [unsafe-row]
                                   (->> (without-one-element unsafe-row)
                                        (some row-safe?))) unsafe)]
    (+ (count safe) (count safe-unsafe-rows))))

(safeish-lines test-data)

; Brute-force-løsningen kjører heldigvis på beskjedne 17ms på
; inputten i dag, så vi slipper å optimalisere denne gangen

(comment
  (= 373 (safeish-lines (slurp "input/2024/input2.txt"))))
