(ns y2022.d10
  (:require [advent-of-code-clj.utils :refer [sum]]
            [clojure.string :as str]))

(defn delay-operation [[op :as operation]]
  (condp = op
    :noop [operation]
    :addx [[:noop nil] operation]))

(defn cycle-seq [data]
  (->> data
       str/split-lines
       (map (fn [line]
              (let [[op arg] (str/split line #"\s")]
                [(keyword op) (some-> arg parse-long)])))
       (mapcat delay-operation)))

(defn process-cycles [cs]
  (reductions (fn [acc [op arg]]
                (let [uacc (-> acc
                               (update :X + (:to-add acc))
                               (assoc :to-add 0))]
                  (case op
                    :noop uacc
                    :addx (assoc uacc :to-add arg))))
              {:X 1
               :to-add 0} cs))

(defn part-1 [data]
  (let [x-values (map :X (process-cycles (cycle-seq data)))]
    (->> [20 60 100 140 180 220]
         (map (fn [x] (* x (nth x-values x))))
         sum)))

(defn part-2 [data]
  (->> data
       cycle-seq
       process-cycles
       (map :X)
       rest ;; Drop the initial position
       (map-indexed vector)
       (reduce (fn [acc [n x]]
                 ;; x is the position of a sprite 3 pixels wide
                 ;; so if n is within the sprite, draw a #
                 (conj acc (if (>= (inc x) (mod n 40) (dec x))
                             ;; Inspired by reddit to make 
                             ;; the text way more readable
                             \█ \space)))
               [])
       (partition 40)
       (interpose "\n")
       flatten
       (apply str)))

(comment
  (= 17380 (part-1 (slurp "input/2022/10.txt")))
  (= "████  ██   ██  █  █ ████ ███  ████  ██  
█    █  █ █  █ █  █    █ █  █ █    █  █ 
███  █    █    █  █   █  █  █ ███  █    
█    █ ██ █    █  █  █   ███  █    █    
█    █  █ █  █ █  █ █    █ █  █    █  █ 
█     ███  ██   ██  ████ █  █ ████  ██  "
     (doto (part-2 (slurp "input/2022/10.txt")) println)))