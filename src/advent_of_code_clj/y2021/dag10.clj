(ns advent-of-code-clj.y2021.dag10
  (:require [clojure.string :as str]))

(defn parse [t]
  (str/split-lines t))

(def test-data (parse "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(def ending-delimiter #{\) \] \} \>})
(def pairs {\( \) \[ \] \{ \} \< \>})
(def bad-scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})
(def incomplete-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn middle-value [xs]
  (nth xs (/ (count xs) 2)))

(defn add [nesting ch]
  (if (ending-delimiter ch)

    (if (not= ch (pairs (last nesting)))
      (throw (ex-info "Bad char" {:char ch
                                  :nesting nesting}))
      (pop nesting))

    (conj nesting ch)))

(defn process-line [line]
  (try (loop [[h & rest] line
              nesting []]
         (if h
           (recur rest (add nesting h))
           nesting))
       (catch Exception e
         (ex-data e))))

(defn calc-incomplete-score [nesting]
  (loop [score 0
         [h & tail] (->> nesting
                         reverse
                         (map pairs))]
    (if h
      (recur (+ (* score 5)
                (incomplete-scores h))
             tail)
      score)))

(assert (->> test-data
             (map process-line)
             (keep :char)
             (map bad-scores)
             (apply +)
             (= 26397)))

(assert (->> test-data
             (map process-line)
             (remove :char)
             (map calc-incomplete-score)
             sort
             middle-value
             (= 288957)))

(comment
  (->> (parse (slurp "input/y2021/day10-input.txt"))
       (map process-line)
       (keep :char)
       (map bad-scores)
       (apply +)
       (= 168417))

  (->> (parse (slurp "input/y2021/day10-input.txt"))
       (map process-line)
       (remove :char)
       (map calc-incomplete-score)
       sort
       middle-value
       (= 2802519786))
  ;;
  )