(ns advent-of-code-clj.y2021.d10
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

(def pairs {\( \) \[ \] \{ \} \< \>})
(def ending-delimiter (set (vals pairs)))
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
  (try
    (reduce (fn [acc ch]
              (add acc ch)) [] line)
    (catch Exception e
      (ex-data e))))

(defn calc-incomplete-score [nesting]
  (reduce (fn [acc x]
            (+ (* 5 acc) x)) 0
          (map (comp incomplete-scores pairs)
               (reverse nesting))))

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
  (->> (parse (slurp "input/y2021/10.txt"))
       (map process-line)
       (keep :char)
       (map bad-scores)
       (apply +)
       (= 168417))

  (->> (parse (slurp "input/y2021/10.txt"))
       (map process-line)
       (remove :char)
       (map calc-incomplete-score)
       sort
       middle-value
       (= 2802519786))
  ;;
  )