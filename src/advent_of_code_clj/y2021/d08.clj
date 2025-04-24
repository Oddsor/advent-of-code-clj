(ns advent-of-code-clj.y2021.d08
  (:require [clojure.set :as set]
            [clojure.string :as str]))

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

(defn to-pattern->number [patterns]
  (let [{twos 2 threes 3 fours 4 fives 5 sixes 6 sevens 7}
        (group-by count (map set patterns))

        [n1 n4 n7 n8]
        (map first [twos fours threes sevens])

        n9 (->> sixes
                (filter (partial set/subset? n4))
                first)
        n0 (->> sixes
                (remove #{n9})
                (filter (partial set/subset? n1))
                first)
        n6 (->> sixes (remove #{n9 n0}) first)

        n3 (->> fives (filter (partial set/subset? n1)) first)
        n5 (->> fives (filter (partial set/superset? n6)) first)
        n2 (->> fives (remove #{n3 n5}) first)]
    {n0 0 n1 1 n2 2 n3 3 n4 4
     n5 5 n6 6 n7 7 n8 8 n9 9}))

(defn process-line [[num-patterns nums]]
  (let [pattern->number (to-pattern->number num-patterns)]
    (map (comp pattern->number set) nums)))

(defn count-simple-nums [data]
  (count (mapcat #(filter #{1 4 7 8} %) data)))

(assert (= 26 (->> test-data
                   (map process-line)
                   count-simple-nums)))

(defn xs->number [xs]
  (parse-long (apply str xs)))

(assert (= 61229 (->> test-data
                      (map (comp xs->number process-line))
                      (apply +))))

(comment
  (= 479
     (->> (parse (slurp "input/y2021/08.txt"))
          (map process-line)
          count-simple-nums))
  (= 1041746
     (->> (parse (slurp "input/y2021/08.txt"))
          (map (comp xs->number process-line))
          (apply +)))
  ;;
  )