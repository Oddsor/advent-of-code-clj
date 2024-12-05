(ns y2024.d05)

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

(defn parse-input [input]
  (let [[rules seqs] (.split input "\n\n")
        rulesets (->> (.split rules "\n")
                      (map #(let [[k v] (.split % "\\|")]
                              {k #{v}}))
                      (apply merge-with into))
        seqs (map #(.split % ",") (.split seqs "\n"))]
    [rulesets seqs]))

(parse-input test-input)

(defn valid-seq? [rulesets s]
  (loop [[h & t] s]
    (cond
      (nil? h) true
      (every? (rulesets h #{}) t) (recur t)
      :else false)))

(defn sum-middle-pages [xs]
  (->> xs
       (map (fn [x] (parse-long (nth x (/ (count x) 2)))))
       (reduce +)))

(defn part-1 [input]
  (let [[rulesets seqs] (parse-input input)]
    (->> seqs
         (filter #(valid-seq? rulesets %))
         sum-middle-pages)))

(= 143 (part-1 test-input))

(comment
  (= 4905 (part-1 (slurp "input/2024/input5.txt"))))

(defn unordered-seqs [rulesets xs]
  (->> xs
       (remove #(valid-seq? rulesets %))))

(defn part-2 [input]
  (let [[rulesets seqs] (parse-input input)
        comparer (fn [a b]
                   (some? ((rulesets a #{}) b)))]
    (->> (unordered-seqs rulesets seqs)
         (map #(sort comparer %))
         sum-middle-pages)))

(= 123 (part-2 test-input))

(comment
  (= 6204 (part-2 (slurp "input/2024/input5.txt"))))
