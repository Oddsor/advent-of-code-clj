(ns advent-of-code-clj.y2022.d06)

(defn unique-sequence-end-index [length data]
  (let [data-vec (vec data)]
    (reduce (fn [acc n]
              (if (= length (count (set (subvec data-vec (- n length) n))))
                (reduced n)
                acc))
            nil (range length (count data-vec)))))

(defn part-1 [data]
  (unique-sequence-end-index 4 data))

(defn part-2 [data]
  (unique-sequence-end-index 14 data))

(comment (part-1 (slurp "input/2022/06.txt"))
         (part-2 (slurp "input/2022/06.txt")))