^:kindly/hide-code
(ns y2025.d09
  (:require
    [advent-of-code-clj.input :as input] 
    [clojure.math.combinatorics :as combinatorics]))

; # 2025, dag 9

; ## Del 1

; Dagens oppgave starter tilsynelatende ganske enkelt, vi skal finne det største mulige rektangelet 
; mellom to av X mulige punkter. For eksempel testdataene:

(def test-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defn points [input]
  (->> input
       (re-seq #"(\d+),(\d+)")
       (map (fn [[_ & coords]]
              (mapv parse-long coords)))))

(points test-input)

; I test-inputten skal største rektangel være mellom 2,5 og 11,1. Her er det
; bare å kalkulere alle areal og sortere:

(defn area [[x1 y1] [x2 y2]]
  (* (inc (abs (- y2 y1))) (inc (abs (- x2 x1)))))

(defn with-area [points]
  (->> (combinatorics/combinations points 2)
       (map (fn [x] [(apply area x) x]))))

(defn part-1 [input]
  (->> input
       points
       with-area
       (sort-by first >)
       first))

(part-1 test-input)

(part-1 (input/get-input 2025 9))