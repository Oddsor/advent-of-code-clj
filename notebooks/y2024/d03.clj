^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2024.d03)

; # 2024, dag 3

; ## Del 1

; Starter med en enkel regex for å finne alle gyldige
; multiplikasjoner:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-and-multiply [text]
  (transduce (map (fn [[_ num1 num2]]
                    (* (Integer/parseInt num1)
                       (Integer/parseInt num2))))
             +
             (re-seq #"mul\((\d+),(\d+)\)" text)))

(= 161 (parse-and-multiply "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (parse-and-multiply (slurp "input/2024/input3.txt")))

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
             (re-seq #"(don't\(\).*?do\(\)|(mul)\((\d+),(\d+)\))" text)))

(= 48 (parse-and-multiply-when "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (parse-and-multiply-when (slurp "input/2024/input3.txt")))

; Reell input ble regnet feil!? Da går vi dumt til verks.

; Hent alle operasjoner vi bryr oss om:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-operations [text]
  (re-seq #"(don't\(\)|do\(\)|mul\((\d+),(\d+)\))" text))

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

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (process-operations (get-operations (slurp "input/2024/input3.txt"))))
