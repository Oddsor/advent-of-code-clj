^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2024.d05
  (:require
   [advent-of-code-clj.input :as input]))

; # 2024, dag 5

; Dagens oppgave var vanskelig å tyde, men i korte trekk skal
; vi bruke et regelsett for å avgjøre om et siffer skal forekomme
; foran eller bak et annet siffer, og bruke dette til å finne
; gyldige kombinasjoner.

; Start med parsing:

(def test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-input [input]
  (let [[rules seqs] (.split input "\n\n")
        rulesets (->> (.split rules "\n")
                      (map #(let [[k v] (.split % "\\|")]
                              {k #{v}}))
                      (apply merge-with into))
        seqs (map #(.split % ",") (.split seqs "\n"))]
    [rulesets seqs]))

(parse-input test-input)

; Regelsettet vårt er en enkel hashmap som inneholder et sett
; av siffer som skal forekomme bak i sekvensen.

; Altså er en gyldig sekvens en sekvens hvor resten av sekvensen
; er til stede i settet for hvert siffer.

; Eksempel:
(let [[h & t] ["29" "13"]
      ruleset {"29" #{"13"}}]
  (every? (ruleset h #{}) t))

(let [[h & t] ["29" "13"]
      ruleset {"13" #{"29"}}]
  (every? (ruleset h #{}) t))

; Dermed kan vi loope igjennom hvert siffer og sjekke
; om resterende siffer er inkludert i settet for hvert siffer:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn valid-seq? [rulesets s]
  (loop [[h & t] s]
    (cond
      (nil? h) true
      (every? (rulesets h #{}) t) (recur t)
      :else false)))

(valid-seq? {"47" #{"61" "53" "13" "29"},
             "97" #{"61" "47" "53" "13" "75" "29"},
             "75" #{"61" "47" "53" "13" "29"},
             "61" #{"53" "13" "29"},
             "29" #{"13"},
             "53" #{"13" "29"}}
            ["75", "47", "61", "53", "29"])

(valid-seq? {"47" #{"61" "53" "13" "29"},
             "97" #{"61" "47" "53" "13" "75" "29"},
             "75" #{"61" "47" "53" "13" "29"},
             "61" #{"53" "13" "29"},
             "29" #{"13"},
             "53" #{"13" "29"}}
            ["75", "97", "47", "61", "53"])

^{:nextjournal.clerk/visibility {:result :hide}}
(defn sum-middle-pages [xs]
  (->> xs
       (map (fn [x] (parse-long (nth x (/ (count x) 2)))))
       (reduce +)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [input]
  (let [[rulesets seqs] (parse-input input)]
    (->> seqs
         (filter #(valid-seq? rulesets %))
         sum-middle-pages)))

(= 143 (part-1 test-input))

(= 4905 (part-1 (input/get-input 2024 5)))

; Morsom innsikt: hashmappen som inneholder "alle sider 
; som skal være bak denne siden", kan gjenbrukes som en
; comparator, slik at vi rett og slett kan sortere på den!

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [input]
  (let [[rulesets seqs] (parse-input input)
        comparer (fn [a b]
                   (some? ((rulesets a #{}) b)))]
    (->> seqs
         (remove #(valid-seq? rulesets %))
         (map #(sort comparer %))
         sum-middle-pages)))

(= 123 (part-2 test-input))

(= 6204 (part-2 (input/get-input 2024 5)))
