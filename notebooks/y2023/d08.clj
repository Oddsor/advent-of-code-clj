(ns y2023.d08
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as nt]))

(def test-data "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")
(def test-data-2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defn steps-needed [from->to directions start-step goals at]
  (let [dirlength (count directions)]
    (loop [at at
           steps start-step]
      (let [nstep (inc steps)
            next-at (get (get from->to at)
                         (get directions (mod steps dirlength)))]
        (if (goals at)
          [steps (goals at)]
          (recur next-at nstep))))))

(defn part-1 [data]
  (let [[dirs pairs] (str/split data #"\n\n")
        directions (mapv {\L 0 \R 1} (seq dirs))
        from->to (->> (str/split-lines pairs)
                      (map (fn [x]
                             (let [[from l r] (re-seq #"\w+" x)]
                               [from [l r]])))
                      (into {}))]
    (first (steps-needed from->to directions 0 #{"ZZZ"} "AAA"))))

(= 2 (part-1 test-data))
(= 6 (part-1 test-data-2))
(comment
  (= 18727 (part-1 (slurp "input/2023/d08.txt"))))

(def test-data-3 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn part-2 [data]
  (let [[dirs pairs] (str/split data #"\n\n")
        directions (mapv {\L 0 \R 1} (seq dirs))
        from->to (->> (str/split-lines pairs)
                      (map (fn [x]
                             (let [[from l r] (re-seq #"\w+" x)]
                               [from [l r]])))
                      (into {}))]
    (->> (keys from->to)
         (filterv #(str/ends-with? % "A"))
         (map (fn [at]
                (reduce (fn [acc x]
                          (if (acc x)
                            (reduced acc)
                            (conj acc x))) #{}
                        (iterate (fn [[s x]]
                                   (steps-needed from->to
                                                 directions
                                                 s
                                                 (set (filterv #(str/ends-with? % "Z") (keys from->to)))
                                                 x))
                                 [0 at]))))
         (map (fn [x] (apply max (map first x))))
         (reduce nt/lcm 1))))

(= 6 (part-2 test-data-3))
(comment
  (= 18024643846273
     (part-2 (slurp "input/2023/d08.txt"))))