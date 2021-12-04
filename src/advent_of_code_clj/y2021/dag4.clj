(ns advent-of-code-clj.y2021.dag4
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]))

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


(defn transpose
  "(transpose [[1 2] [3 4]])
   => '((1 3) (2 4]))"
  [seq-of-seqs]
  (apply map list seq-of-seqs))

(defn parse [text]
  (let [[draw-seq & lines] (str/split-lines text)]
    {:draw-seq (read-numbers draw-seq)
     :boards (->> lines
                  (partition-by #{""})
                  (remove #{'("")})
                  (map (fn [seq-of-nums]
                         (mapv read-numbers seq-of-nums))))}))

(defn board-wins? [board]
  (let [horizontal-sets (map set board)
        vertical-sets (map set (transpose board))]
    (boolean (or (some #{#{nil}} horizontal-sets)
                 (some #{#{nil}} vertical-sets)))))

(defn remove-value-from-board [num board]
  (s/setval (s/walker #{num}) nil board))

(defn determine-winner [or-loser {:keys [draw-seq boards]}]
  (reduce (fn [boards drawn-num]
            (let [{winners true
                   losers false} (group-by board-wins? (map (partial remove-value-from-board drawn-num) boards))]
              (if-let [board (and (if or-loser
                                    (= 1 (count boards))
                                    true)
                                  (first winners))]
                (reduced {:drawn-num drawn-num
                          :winning-sum (* drawn-num (apply + (s/select (s/walker number?) board)))
                          :winning-board board})
                losers)))
          boards draw-seq))
(assert (= 4512 (:winning-sum (determine-winner false (parse test-data)))))
(assert (= 1924 (:winning-sum (determine-winner true (parse test-data)))))

(comment
  (determine-winner false (parse (slurp "input/y2021/day4-input.txt")))
  (determine-winner true (parse (slurp "input/y2021/day4-input.txt"))))