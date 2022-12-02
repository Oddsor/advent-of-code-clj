(ns advent-of-code-clj.y2022.d02 
  (:require [clojure.string :as str]))

(def point-table {"A X" (+ 1 3)
                  "A Y" (+ 2 6)
                  "A Z" (+ 3 0)
                  "B X" (+ 1 0)
                  "B Y" (+ 2 3) 
                  "B Z" (+ 3 6)
                  "C X" (+ 1 6)
                  "C Y" (+ 2 0)
                  "C Z" (+ 3 3)})

(defn part-1 [data]
  (apply + (map point-table (str/split-lines data))))

(def point-table-2 {"A X" (+ 3 0)
                    "A Y" (+ 1 3)
                    "A Z" (+ 2 6)
                    "B X" (+ 1 0)
                    "B Y" (+ 2 3)
                    "B Z" (+ 3 6)
                    "C X" (+ 2 0)
                    "C Y" (+ 3 3)
                    "C Z" (+ 1 6)})

(defn part-2 [data]
  (apply + (map point-table-2 (str/split-lines data))))

(comment 
  (part-1 (slurp "input/2022/02.txt"))
  (part-2 (slurp "input/2022/02.txt")))
