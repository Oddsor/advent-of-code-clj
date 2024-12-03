^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2024.d03)

; # 2024, dag 3

; ## Del 1

; Starter med en enkel regex for å finne alle gyldige
; multiplikasjoner. Denne gangen tester vi om regal-biblioteket
; gjør ting mer lesbart..?

(require '[lambdaisland.regal :as regal])
(def mul-pattern
  (regal/regex [:cat "mul("
                [:capture [:+ :digit]] ","
                [:capture [:+ :digit]] ")"]))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-and-multiply [text]
  (transduce (map (fn [[_ num1 num2]]
                    (* (Integer/parseInt num1)
                       (Integer/parseInt num2))))
             +
             (re-seq mul-pattern text)))

(= 161 (parse-and-multiply "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 173419328 (parse-and-multiply (slurp "input/2024/input3.txt"))))

; ## Del 2

; Her ble det brått litt mer komplisert, vi må filtrere vekk
; alle multiplikasjoner som er mellom en `don't()` og `do()`.

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-and-multiply-when [text]
  (transduce (comp
              (filter (fn [[_ _ x]] (= "mul" x)))
              (map (fn [[_ _ _ num1 num2]]
                     (* (Integer/parseInt num1)
                        (Integer/parseInt num2)))))
             +
             (re-seq (regal/regex [:capture
                                   [:alt
                                    [:cat "don't()" [:*? :any] "do()"]
                                    [:cat [:capture "mul"] "(" [:capture [:+ :digit]] "," [:capture [:+ :digit]] ")"]]])
                     text)))

(= 48 (parse-and-multiply-when "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 98059175 (parse-and-multiply-when (slurp "input/2024/input3.txt"))))

; Men reell input ble regnet feil!! Da får vi gå dumt til verks.

; Hent alle operasjoner vi bryr oss om:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-operations [text]
  (re-seq (regal/regex [:capture
                        [:alt
                         "don't()"
                         "do()"
                         [:cat "mul("
                          [:capture [:+ :digit]] ","
                          [:capture [:+ :digit]] ")"]]]) text))

(get-operations "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

; Så behandler vi operasjonene rekursivt, og holder på en toggle
; som avgjør om en multiplikasjon skal utføres:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn process-operations [operations]
  (loop [[op & rops] operations
         do? true
         sum 0]
    (if op
      (cond
        (= "don't()" (first op)) (recur rops false sum)
        (= "do()" (first op)) (recur rops true sum)
        :else (recur rops do? (if do?
                                (+ sum (* (Integer/parseInt (op 2))
                                          (Integer/parseInt (op 3))))
                                sum)))
      sum)))

(= 48 (process-operations (get-operations "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")))

; Fortsatt riktig på testdata, og nå blir det også riktig på inputten:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 90669332 (process-operations (get-operations (slurp "input/2024/input3.txt")))))

; ## Del 2 igjen

; Etter å ha sett løsningen til https://github.com/zelark/, så kan
; det være enklere å "preppe" inputten og gjenbruke funksjonen fra del 1.

; Det første vi ser er at regal ikke støtter enkelte operasjoner (`?s` som
; gjør at `.` matcher newline, og `\Z` som gjør at vi matcher slutten av filen)

(require '[lambdaisland.regal.parse :as rp])
(rp/parse-pattern "(?s)don't\\(\\).*?(?:do\\(\\)|\\Z)")

; Men det kan vi løse ved å legge til newline sammen med `any`-operatoren
(def prep-pattern (regal/pattern [:cat
                                  "don't()"
                                  [:*? [:alt :any :newline]]
                                  [:alt "do()" :end]]))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn prep-input [input]
  ; ?s slik at `.` matcher newline også
  ;`\Z` for å matche slutten av filen (fanger opp `don't` som ikke følges av en `do`)
  (.replaceAll input prep-pattern ""))

(prep-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?don't()mul(8,5))")

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 90669332 (parse-and-multiply (prep-input (slurp "input/2024/input3.txt")))))
