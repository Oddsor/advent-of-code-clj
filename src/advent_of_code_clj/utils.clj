(ns advent-of-code-clj.utils
  (:require [clojure.string :as str]))

(defn coord-map [xs-of-xses]
  (->> xs-of-xses
       (map-indexed (fn [idy xs]
                      (map-indexed (fn [idx v]
                                     [[idx idy] v])
                                   xs)))
       (transduce cat merge)))

(defn adjacent-hv
  "Find adjacent coordinates, without diagonals"
  [x y]
  [[x (dec y)]
   [(dec x) y] [(inc x) y]
   [x (inc y)]])

(defn adjacent
  "Find adjacent coordinates"
  [x y]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y]                   [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn partition-parse
  ([text]
   (partition-parse identity text))
  ([line-parsing-fn text]
   (->> (str/split text #"\n\n")
        (map str/split-lines)
        (map #(map line-parsing-fn %)))))

(defn fif [pred fun]
  #(if (pred %) (fun %) %))