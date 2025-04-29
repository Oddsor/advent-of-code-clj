(ns y2024.d24
  (:require
    [advent-of-code-clj.input :as input])
  (:import
    [java.util Collection]))

(def test-input "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(defn parse-logic-statement [logic-statement]
  (let [[v1 op v2 res] (re-seq #"\w+" logic-statement)
        op-fn (case op
                "AND" bit-and
                "OR" bit-or
                "XOR" bit-xor)]
    {:op op-fn
     :inputs [v1 v2]
     :res res}))

(parse-logic-statement "hwm AND bqk -> z03")

(defn solve-expressions [input]
  (let [[initial-vars logic-statements] (map String/.trim (String/.split input "\n\n"))]
    (loop [ls (map parse-logic-statement (String/.split logic-statements "\n"))
           known-vals (into {} (map (fn [line]
                                      (let [[id val] (map String/.trim (String/.split line ":"))]
                                        [id (parse-long val)]))
                                    (String/.split initial-vars "\n")))]

      (let [solvable (filter #(every? known-vals (:inputs %)) ls)
            unsolvable (filter #(not-every? known-vals (:inputs %)) ls)
            new-results (reduce (fn [acc {:keys [inputs op res]}]
                                  (assoc acc res
                                         (apply op (map known-vals inputs))))
                                known-vals
                                solvable)]
        (if (empty? unsolvable)
          new-results
          (recur unsolvable new-results))))))

(defn part-1 [input]
  (let [results (solve-expressions input)
        z-vals (reverse (sort-by key (filter #(String/.startsWith (key %) "z") results)))]
    (read-string (str "2r" (String/join "" ^Collection (mapv (comp str val) z-vals))))))

(part-1 test-input)

(part-1 (input/get-input 2024 24))
