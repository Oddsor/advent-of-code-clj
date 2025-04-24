(ns advent-of-code-clj.y2020.d08
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]))

(def test-data "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-instructions [text]
  (mapv (comp (fn [[op v]]
                {:op (keyword op)
                 :value (parse-long v)}) #(str/split % #"\s"))
        (str/split-lines text)))

(defn process-instructions [instruction-set]
  (loop [position 0
         acc 0
         previous-positions #{}]
    (let [visited-positions (conj previous-positions position)
          {:keys [op value]} (nth instruction-set position)
          [next-pos next-acc] (condp = op
                                :nop [(inc position) acc]
                                :acc [(inc position) (+ acc value)]
                                :jmp [(+ position value) acc])]
      (cond
        ;; Hvis programmet kjørte til slutten
        ;; uten å sendes out of bounds via jmp,
        ;; så er kjøring vellykket
        (and (= next-pos (count instruction-set))
             (= next-pos (inc position)))
        [:ok next-acc]
        ;; Hvis en instruksjon er besøkt tidligere
        ;; eller neste posisjon er out of bounds, halt
        (or (= visited-positions previous-positions)
            (not (>= (dec (count instruction-set)) next-pos 0)))
        [:halt acc]
        ;; Kjør neste instruksjon
        :else
        (recur next-pos next-acc visited-positions)))))

(assert (= [:halt 5] (process-instructions (parse-instructions test-data))))

(comment
  (process-instructions (parse-instructions (slurp "input/y2020/08.txt"))))

(def flip-op {:jmp :nop
              :nop :jmp})
(def ops-to-transform (set (keys flip-op)))

(defn variations [instruction-set]
  (let [relevant-instruction? (comp ops-to-transform :op)
        num-variations (count (filter relevant-instruction? instruction-set))]
    (->> (range num-variations)
         (map (fn [index]
                (s/transform
                  [(s/filterer relevant-instruction?) (s/nthpath index) :op]
                  flip-op
                  instruction-set)))
         (cons instruction-set))))

(defn valid-results [instruction-set]
  (->> instruction-set
       variations
       (map process-instructions)
       (filter (comp #{:ok} first))))

(assert (= [[:ok 8]] (valid-results (parse-instructions test-data))))

(comment
  (valid-results (parse-instructions (slurp "input/y2020/08.txt"))))