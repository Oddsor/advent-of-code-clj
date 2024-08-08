(ns y2023.d15
  (:require [clojure.string :as str]
            [flatland.ordered.map :refer [ordered-map]]))

(def test-data "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn hash-symbols [xs]
  (reduce (fn [acc x]
            (let [ascii-value (int x)]
              (mod (* (+ acc ascii-value) 17) 256))) 0 xs))

(= 1320 (apply + (map hash-symbols
                      (str/split test-data #","))))

(comment
  (= 515210 (apply + (map hash-symbols
                          (str/split (str/replace (slurp "input/2023/d15.txt") "\n" "") #",")))))

(defn part-2 [data]
  (let [boxes (reduce
               (fn [acc syms]
                 (let [[_ idstring op num] (first (re-seq #"(\w+)([=-])(\d+)?" syms))
                       hsh (hash-symbols idstring)]
                   (case (first op)
                     \= (update acc hsh (fn [x]
                                          (assoc (or x (ordered-map)) idstring num)))
                     \- (update acc hsh dissoc idstring))))
               {}
               (str/split (str/trim data) #","))]
    (transduce (mapcat (fn [[boxn items]]
                         (map-indexed (fn [idx [_ i]] (* (inc boxn) (inc idx) (parse-long i)))
                                      items))) +
               boxes)))

(= 145 (part-2 test-data))
(comment
  (= 246762 (part-2 (slurp "input/2023/d15.txt"))))