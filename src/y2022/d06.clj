(ns y2022.d06
  (:require [tech.v3.datatype.argops :as da]
            [tech.v3.datatype.rolling :as dr]))

(defn unique-sequence-end-index [length data]
  (->> data
       (partition length 1)
       (take-while (partial apply (complement distinct?)))
       count (+ length)))

(def ^:dynamic *unique-sequence-end-index-fn* unique-sequence-end-index)

(defn part-1 [data]
  (*unique-sequence-end-index-fn* 4 data))

(defn part-2 [data]
  (*unique-sequence-end-index-fn* 14 data))

(comment (= 1651 (part-1 (slurp "input/2022/06.txt")))
         (crit/quick-bench
           (= 3837 (part-2 (slurp "input/2022/06.txt"))))
         ;; Execution time mean : 10,199115 ms
         )
(defn unique-sequence-end-index-2 [length data]
  (-> (seq data)
      (dr/fixed-rolling-window length
                               (partial apply distinct?)
                               {:relative-window-position :right})
      (da/index-of true)
      (+ length)))

(comment
  (with-bindings {#'*unique-sequence-end-index-fn* unique-sequence-end-index-2}
    (= 1651 (part-1 (slurp "input/2022/06.txt"))))
  (crit/quick-bench
    (with-bindings {#'*unique-sequence-end-index-fn* unique-sequence-end-index-2}
      (= 3837 (part-2 (slurp "input/2022/06.txt")))))
  ;; Execution time mean : 5,866074 ms
  )
