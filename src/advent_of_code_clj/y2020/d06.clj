(ns advent-of-code-clj.y2020.d06
  (:require [advent-of-code-clj.utils :refer [split-newline]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn questions-answered-by-group [group]
  (count (set (mapcat identity group))))

(defn questions-answered-by-everyone-in-group [group]
  (count (apply set/intersection (map set group))))

(defn parse [data]
  (->> data split-newline (map str/split-lines)))

(defn part-1 [data]
  (reduce + (map questions-answered-by-group (parse data))))

(defn part-2 [data]
  (reduce + (map questions-answered-by-everyone-in-group (parse data))))

(comment
  ; Del 1
  (= 6930 (part-1 (slurp "input/2020/06.txt")))

  ; Del 2
  (= 3585 (part-2 (slurp "input/2020/06.txt"))))