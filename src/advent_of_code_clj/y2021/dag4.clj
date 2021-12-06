(ns advent-of-code-clj.y2021.dag4
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]
            [clojure.core.matrix :as m]))

(def test-data "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn read-numbers
  "\"14 21 17 24  4\" => (14 21 17 24 4)"
  [text]
  (map read-string (re-seq #"\d+" text)))

(defn parse [text]
  (let [[draw-seq & lines] (str/split-lines text)]
    {:draw-seq (read-numbers draw-seq)
     :boards (->> lines
                  (partition-by #{""})
                  (remove #{'("")})
                  (map (fn [seq-of-nums]
                         (mapv read-numbers seq-of-nums))))}))

(defn board-wins? [board]
  ;; Mer elegant algo fra tschady: https://github.com/tschady/advent-of-code/blob/main/src/aoc/2021/d04.clj
  (some #(every? nil? %) (concat board (m/transpose board))))

(defn remove-value-from-board [num board]
  (s/setval (s/walker #{num}) nil board))

;; Mer elegant rekursjon via lazy-seq, også inspirert av tschady (og hele clojure core)
(defn scores [[num & nums] boards]
  (lazy-seq
   (when num
     (let [{winners true
            losers nil} (group-by board-wins? (map (partial remove-value-from-board num) boards))]
       (if (seq winners)
         (cons {:score (* num (reduce + (s/select (s/walker number?) (first winners))))
                :winning-boards winners}
               (scores nums losers))
         (scores nums losers))))))

(assert (= 4512 (:score (first (scores (:draw-seq (parse test-data))
                                       (:boards (parse test-data)))))))
(assert (= 1924 (:score (last (scores (:draw-seq (parse test-data))
                                      (:boards (parse test-data)))))))

(comment
  (= 74320 (:score (first (apply scores ((juxt :draw-seq :boards) (parse (slurp "input/y2021/day4-input.txt")))))))
  (= 17884 (:score (last (apply scores ((juxt :draw-seq :boards) (parse (slurp "input/y2021/day4-input.txt")))))))
  ;;
  )