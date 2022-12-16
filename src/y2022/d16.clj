(ns y2022.d16
  (:require [clojure.string :as str]
            [ubergraph.alg :as ua]
            [clojure.set :as set]
            [clojure.math.combinatorics :as c]))

(def test-data "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(defn parse [data]
  (->> data
       str/split-lines
       (map (fn [line]
              (let [tokens (re-seq #"\w+" line)
                    [valve & connected-valves] (keep (fn [x]
                                                       (when (re-matches #"[A-Z]+" x)
                                                         (keyword x)))
                                                     tokens)
                    pressure (parse-long (some (partial re-matches #"\d+")
                                               tokens))]
                [valve {:next connected-valves
                        :pressure pressure}])))
       (into {})))

(defn shortest-path [start-pos end-pos data]
  (ua/shortest-path (fn [coord]
                      (map (fn [x] {:dest x})
                           (:next (coord data))))
                    {:start-node start-pos
                     :end-node end-pos}))

(defn part-1 [data]
  (let [d (parse data)
        sp (memoize shortest-path)
        op (set (remove (fn [valve] (zero? (:pressure (valve d)))) (keys d)))
        start :AA
        calc-pressure (fn [path]
                        (reduce (fn [acc [valve activated-at]]
                                  (if (pos-int? (- 30 activated-at))
                                    (+ acc (* (:pressure (valve d)) (- 30 activated-at)))
                                    acc))
                                0 (map vector (:visited path) (:step path))))]
    (loop [n 30
           paths [{:visited [start] :step [0]}]]
      (let [np (mapcat (fn [path]
                         (if (> (last (:step path)) 30)
                           [path]
                           (if-let [next-nodes (seq (set/difference op (set (:visited path))))]
                             (map (fn [end]
                                    (let [s (sp (last (:visited path)) end d)]
                                      {:visited (conj (:visited path) end)
                                       :step (conj (:step path) (+ (last (:step path))
                                                                   (inc (:cost s))))}))
                                  next-nodes)
                             [path])))
                       paths)]
        (if (or (= np paths) (zero? n))
          (apply max (map calc-pressure paths))
          (recur (dec n)
                 np))))))

(assert (= 1651 (part-1 test-data)))
(comment
  (part-1 (slurp "input/2022/16.txt")))