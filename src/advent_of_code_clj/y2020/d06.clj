(ns advent-of-code-clj.y2020.d06
  (:require [advent-of-code-clj.utils :refer [partition-parse]]
            [clojure.set :as set]))

(defn questions-answered-by-group [group]
  (count (set (mapcat identity group))))

(defn questions-answered-by-everyone-in-group [group]
  (count (apply set/intersection (map set group))))

(defn part-1 [data]
  (reduce + (map questions-answered-by-group (partition-parse data))))

(defn part-2 [data]
  (reduce + (map questions-answered-by-everyone-in-group (partition-parse data))))

(comment
  ; Del 1
  (part-1 (slurp "input/y2020/06.txt"))

  ; Del 2
  (part-2 (slurp "input/y2020/06.txt")))