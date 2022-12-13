(ns advent-of-code-clj.y2022.d06
  (:require [tech.v3.datatype.argops :as da]
            [tech.v3.datatype.rolling :as dr]
            [criterium.core :as crit]))

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
         (crit/quick-bench
          (part-2 (slurp "input/2022/06.txt")))
         ;; Execution time mean : 10,199115 ms
         )
(defn unique-sequence-end-index-2 [length data]
  (-> (seq data)
      (dr/fixed-rolling-window length
                               (partial apply distinct?)
                               {:relative-window-position :right})
      (da/index-of true)
      (+ length)))

(defn part-1-dt [data]
  (unique-sequence-end-index-2 4 data))

(defn part-2-dt [data]
  (unique-sequence-end-index-2 14 data))

(comment
  (part-1-dt (slurp "input/2022/06.txt"))
  (crit/quick-bench
   (part-2-dt (slurp "input/2022/06.txt")))
  ;; Execution time mean : 5,866074 ms
  )