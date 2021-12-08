(ns advent-of-code-clj.y2021.dag8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [text]
  (->> (str/split-lines text)
       (map #(map (partial re-seq #"\w+")
                  (str/split % #" \| ")))))

(def test-data (parse "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(defn manually-deduce-patterns [patterns]
  (let [patterns (map set patterns)
        c->p (into {} (map (juxt count identity))
                   patterns)
        four (c->p 4)
        seven (c->p 3)
        eight (c->p 7)
        one (c->p 2)
        {tr 1
         br 2} (->> patterns
                    (map #(set/difference % (set/difference eight one)))
                    frequencies
                    set/map-invert)
        {bottom 3
         bbl 4} (-> (map #(set/difference % (set/union four seven)) patterns)
                    frequencies
                    (dissoc #{})
                    set/map-invert)
        bl (set/difference bbl bottom)
        mid (->> patterns
                 (map #(set/difference % seven bottom))
                 (filter (comp #{1} count))
                 first)
        tl (set/difference four mid one)]
    {(set/difference eight mid) 0
     one 1
     (set/difference eight tl br) 2
     (set/union seven mid bottom) 3
     four 4
     (set/difference eight tr bl) 5
     (set/difference eight tr) 6
     seven 7
     eight 8
     (set/difference eight bl) 9}))

(defn process-line [[nums num]]
  (let [patterns (manually-deduce-patterns nums)]
    (prn nums)
    (map (comp patterns set) num)))

(defn count-simple-nums [data]
  (count (mapcat #(filter #{1 4 7 8} %) data)))

(assert (= 26 (->> (map process-line test-data)
                   count-simple-nums)))

(comment
  (= 479
     (->> (parse (slurp "input/y2021/day8-input.txt"))
          (map process-line)
          count-simple-nums))
  (= 1041746
     (->> (parse (slurp "input/y2021/day8-input.txt"))
          (map (comp #(Integer/parseInt %) #(apply str %) process-line))
          (apply +))))

