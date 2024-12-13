(ns advent-of-code-clj.utils
  (:require [clojure.string :as str])
  (:import [java.util HashSet]))

(defn coord-map [xs-of-xses]
  (->> xs-of-xses
       (map-indexed (fn [idy xs]
                      (map-indexed (fn [idx v]
                                     [[idx idy] v])
                                   xs)))
       (transduce cat merge)))

(defn coord-map-fixed [xs-of-xses]
  (->> xs-of-xses
       (map-indexed (fn [idy xs]
                      (map-indexed (fn [idx v]
                                     [[idy idx] v])
                                   xs)))
       (transduce cat merge)))

(defn text->matrix [text]
  (mapv vec (str/split-lines text)))

(defn adjacent-hv
  "Find adjacent coordinates, without diagonals"
  ([x y]
   [[x (dec y)]
    [(dec x) y] [(inc x) y]
    [x (inc y)]])
  ([x y z]
   [[(dec x) y z] [(inc x) y z]
    [x (dec y) z]  [x (inc y) z]
    [x y (dec z)] [x y (inc z)]]))

(defn adjacent
  "Find adjacent coordinates"
  [x y]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y]                   [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn split-newline [text]
  (str/split text #"\n\n"))

(defn emap [fun xs]
  (if (coll? xs)
    (into (empty xs) (map #(emap fun %)) xs)
    (fun xs)))

(defn fif [pred fun]
  #(if (pred %) (fun %) %))

(defn sum [xs] (reduce + xs))

(def sort- (partial sort >))

(defn find-neighbours [predicate node]
  (for [neighbour (apply adjacent-hv node)
        :when (predicate neighbour)]
    neighbour))

(defn breadth-search [graph-fn start-node]
  (let [visited (doto (HashSet.) (.add start-node))]
    (iteration (fn [nodes]
                 (if (nil? nodes)
                   [start-node]
                   (let [next-nodes
                         (seq (eduction
                               (mapcat graph-fn)
                               (remove (fn [x]
                                         (.contains visited x)))
                               (distinct)
                               nodes))]
                     (when next-nodes
                       (.addAll visited next-nodes))
                     (seq next-nodes)))))))
