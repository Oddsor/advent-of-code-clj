(ns y2022.d20 
  (:require [clojure.edn :as edn]
            [com.rpl.specter :refer :all]
            [criterium.core :as crit]))

(defn parse [data]
  (edn/read-string (str "[" data "]")))

(defn reorder 
  ([xs] (reorder 1 xs))
  ([times xs]
   (let [size (count xs)
         with-ids (map-indexed (fn [idx n] {:id idx :number n}) xs)]
     (->> (if (> times 1)
            (take (* size times) (cycle with-ids))
            with-ids)
          (reduce
           (fn [acc {n :number :as entry}]
             (if (zero? n)
               acc
               (let [idx (.indexOf acc entry)
                     new-idx (mod (+ idx n) (dec size))]
                 (->> acc
                      (setval [(nthpath idx)] NONE)
                      (setval [(if (zero? new-idx) 
                                 AFTER-ELEM
                                 (before-index new-idx))] entry)
                      vec)))) with-ids)
          (mapv :number)))))

(defn compute [xs]
  (let [zidx (.indexOf xs 0)]
    (transduce (comp (drop zidx)
                     (take-nth 1000)
                     (take 4)) + (cycle xs))))

(defn part-1 [data] (compute (reorder (parse data))))
(defn part-2 [data] (->> data
                         parse
                         (mapv (partial * 811589153))
                         (reorder 10)
                         compute))

(comment
  (part-1 (slurp "input/2022/20.txt"))
  (part-2 (slurp "input/2022/20.txt")))