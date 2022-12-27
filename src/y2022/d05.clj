(ns y2022.d05
  (:require [clojure.string :as str]))

(defn build-stacks [stack-str]
  (->> stack-str
       str/split-lines
       butlast
       (map #(take-nth 4 (rest %)))
       (apply mapv list)
       (mapv #(drop-while #{\space} %))))

(defn operate-on-one [stacks [move-amount from to]]
  (loop [stacks stacks
         n move-amount]
    (if (zero? n)
      stacks
      (recur (-> stacks
                 (update from rest)
                 (update to conj (first (stacks from))))
             (dec n)))))

(defn operate-on-many [stacks [move-amount from to]]
  (let [popped (take move-amount (stacks from))]
    (-> stacks
        (update from #(drop move-amount %))
        (update to #(concat popped %)))))

(defn find-phrase [operation data]
  (let [[stack-str operations-str] (str/split data #"\n\n")]
    (->> (re-seq #"\d+" operations-str)
         (map parse-long)
         (partition 3)
         (map (fn [[move-amount from to]] [move-amount (dec from) (dec to)]))
         (reduce operation (build-stacks stack-str))
         (map first)
         (apply str))))

(defn part-1 [data]
  (find-phrase operate-on-one data))

(defn part-2 [data]
  (find-phrase operate-on-many data))

(comment
  (= "CFFHVVHNC" (part-1 (slurp "input/2022/05.txt")))
  (= "FSZWBPTBG" (part-2 (slurp "input/2022/05.txt"))))