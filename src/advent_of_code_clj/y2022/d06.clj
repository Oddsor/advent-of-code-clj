(ns advent-of-code-clj.y2022.d06)

(defn unique-sequence-end-index [length data]
  (->> data
       (partition length 1)
       (take-while (partial apply (complement distinct?)))
       count (+ length)))

(defn part-1 [data]
  (unique-sequence-end-index 4 data))

(defn part-2 [data]
  (unique-sequence-end-index 14 data))

(comment (part-1 (slurp "input/2022/06.txt"))
         (part-2 (slurp "input/2022/06.txt")))