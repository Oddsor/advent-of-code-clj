^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d03
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [advent-of-code-clj.utils :as u]))

(def test-data "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

^{:nextjournal.clerk/visibility {:result :hide}}
(defn text->matrix [text]
  (->> text str/split-lines (mapv #(str/split % #""))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn is-symbol? [x] (re-matches #"[^\d\.]" x))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn is-digit? [character] (re-matches #"\d" character))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn digit-coordinates [matrix]
  (filter #(is-digit? (get-in matrix %)) (m/index-seq matrix)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn group-number-coordinates [coords]
  (when-let [xs (seq coords)]
    (lazy-seq
     (let [[fst & xss] xs
           grouping (loop [[py px] fst
                           [[ny nx :as n] & xrs] xss
                           acc [fst]]
                      (if (and (= py ny) (= (dec nx) px))
                        (recur n xrs (conj acc n))
                        acc))]
       (cons grouping (group-number-coordinates (drop (count grouping) coords)))))))

(->> test-data text->matrix digit-coordinates group-number-coordinates)

^{:nextjournal.clerk/visibility {:result :hide}}
(defn symbol-coordinates [matrix]
  (into {} (for [y (range (count matrix))
                 x (range (count (get matrix y)))
                 :let [s (get-in matrix [y x])]
                 :when (is-symbol? s)]
             {[y x] s})))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn adjacent-symbol? [symbol-coords [y x]]
  (some symbol-coords (u/adjacent y x)))

(let [matrix (-> test-data text->matrix)
      symbol-coords (symbol-coordinates matrix)]
  [(adjacent-symbol? symbol-coords [3 7])
   (adjacent-symbol? symbol-coords [3 8])])

^{:nextjournal.clerk/visibility {:result :hide}}
(defn filter-number-coords [symbol-coords coords]
  (filter #(some (partial adjacent-symbol? symbol-coords) %) coords))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn coords->numbers
  ([matrix]
   (map (partial map #(get-in matrix %))))
  ([matrix coords]
   (map (partial map #(get-in matrix %)) coords)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [matrix (-> data text->matrix)
        symbol-coords (symbol-coordinates matrix)]
    (->> matrix
         digit-coordinates
         group-number-coordinates
         (filter-number-coords symbol-coords)
         (coords->numbers matrix)
         (map (partial apply str))
         (map parse-long)
         (apply +))))

(= 4361 (part-1 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 544433 (part-1 (slurp "input/2023/d03.txt"))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (let [matrix (-> data text->matrix)
        asterisk-coords (into {} (filter (comp #{"*"} val)) (symbol-coordinates matrix))
        number-coords (-> matrix digit-coordinates group-number-coordinates)
        relevant-coords (->> number-coords (filter-number-coords asterisk-coords))]
    (->> asterisk-coords
         keys
         (map (fn [k]
                (let [adj (set (cons k (apply u/adjacent k)))]
                  (->> relevant-coords
                       (filter #(some adj %))
                       (coords->numbers matrix)
                       (map str/join)
                       (map parse-long)))))
         (filter #(= 2 (count %)))
         (map (partial apply *))
         (apply +))))

(= 467835 (part-2 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 76314915 (part-2 (slurp "input/2023/d03.txt"))))