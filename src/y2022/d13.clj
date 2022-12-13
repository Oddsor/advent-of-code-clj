(ns y2022.d13
  (:require [clojure.edn :as edn]))

(defn in-order? [left right]
  (cond (and (number? left) (number? right)) (compare left right)
        (number? right) (in-order? left (vector right))
        (number? left) (in-order? (vector left) right)
        ;; This solution by @wevre was too good pass up
        :else (or (->> (map in-order? left right) (drop-while zero?) first)
                  (- (count left) (count right)))))

(defn parse [data] (edn/read-string (str "[" data "]")))

(defn part-1 [data]
  (->> (parse data)
       (partition 2)
       (keep-indexed (fn [idx [left right]]
                       (when (neg-int? (in-order? left right)) (inc idx))))
       (apply +)))

(defn part-2 [data]
  (let [dividers #{[[2]] [[6]]}]
    (->> (parse data)
         (concat dividers)
         (sort in-order?)
         (keep-indexed (fn [idx packet]
                         (when (dividers packet)
                           (inc idx))))
         (apply *))))

(comment
  (part-1 (slurp "input/2022/13.txt"))
  (part-2 (slurp "input/2022/13.txt")))