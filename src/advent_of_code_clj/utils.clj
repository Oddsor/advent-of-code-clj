(ns advent-of-code-clj.utils
  (:import [java.util HashSet])
  (:require
    [advent-of-code-clj.matrix :as mx]))

(defn coord-map
  {:malli/schema [:-> [:sequential [:sequential :any]] [:map-of [:tuple :int :int] :any]]}
  [xs-of-xses]
  (->> xs-of-xses
       (map-indexed (fn [idy xs]
                      (map-indexed (fn [idx v]
                                     [[idx idy] v])
                                   xs)))
       (transduce cat merge)))

(def coord-map-fixed mx/coord-map-fixed)

(def text->matrix mx/text->matrix)

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
  (String/.split text "\n\n"))

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

(defn bisect
  "Find the first or last value where a predicate is true in a range.
   
   Tests the start, middle and end of the value range and narrows down
   the search space until a value is found"
  {:malli/schema [:-> ifn? :int [:* :int] :int]}
  ([pred end]
   (bisect pred 0 end))
  ([pred start end]
   (let [middle (quot (+ start end) 2)
         p-start (pred start)
         p-mid (pred middle)
         p-end (pred end)]
     (cond
       (= p-start p-mid p-end) (throw (ex-info "Could not find a solution: start, middle and end had same equality" {}))
       (= start middle) (if p-start start end)
       (and p-mid (not p-end)) (bisect pred middle end)
       (and p-start (not p-mid)) (bisect pred start middle)
       (and (not p-start) p-mid) (bisect pred start middle)
       (and (not p-mid) p-end) (bisect pred middle end)))))

(defn depth-search [graph-fn start-node]
  (let [visited (doto (HashSet.) (.add start-node))]
    (loop [node [start-node]]
      (let [neighbours (graph-fn node)]))))
