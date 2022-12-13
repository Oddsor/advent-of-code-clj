(ns advent-of-code-clj.y2022.d05
  (:require [clojure.string :as str]))

(defn build-stacks [stack-str]
  (->> stack-str
       str/split-lines
       (drop-last 1)
       (map #(take-nth 4 (rest %)))
       (apply mapv list)
       (mapv #(drop-while #{\space} %))))

(defn operate-on-one [stacks [move-amount from to]]
  (loop [stacks stacks
         n move-amount]
    (if (zero? n)
      stacks
      (let [popped (first (stacks (dec from)))]
        (recur (-> stacks
                   (update (dec from) rest)
                   (update (dec to) conj popped))
               (dec n))))))

(defn operate-on-many [stacks [move-amount from to]]
  (let [popped (take move-amount (stacks (dec from)))]
    (-> stacks
        (update (dec from) #(drop move-amount %))
        (update (dec to) #(concat popped %)))))

(defn find-phrase [operation data]
  (let [[stack-str operations-str] (str/split data #"\n\n")
        operations (partition 3 (map parse-long (re-seq #"\d+" operations-str)))
        stack (build-stacks stack-str)]
    (->> (reduce operation stack operations)
         (map first)
         (apply str))))

(defn part-1 [data]
  (find-phrase operate-on-one data))

(defn part-2 [data]
  (find-phrase operate-on-many data))

(comment
  (part-1 (slurp "input/2022/05.txt"))
  (part-2 (slurp "input/2022/05.txt")))