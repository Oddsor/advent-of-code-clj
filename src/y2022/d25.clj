(ns y2022.d25
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def snafu->dec {\= -2
                 \- -1
                 \0 0
                 \1 1
                 \2 2})
(def dec->snafu (set/map-invert snafu->dec))

(defn unsnaf [snafu]
  (reduce (fn [acc [n v]]
            (+ acc
               (* (long (Math/pow 5 n))
                  (snafu->dec v)))) 0
          (map-indexed vector (reverse snafu))))

(defn snaf [dec]
  (let [size (range (int (Math/ceil (/ (Math/log dec) (Math/log 5)))) -1 -1)]
    (->> size
         (reduce (fn [{:keys [acc snaf]} n]
                   (let [[snaf-number multiple] (first (sort-by #(abs (- dec (+ acc (second %))))
                                                                (map (fn [x] [x (* (Math/pow 5 n) x)]) [2 1 0 -1 -2])))]
                     {:acc (+ acc multiple)
                      :snaf (conj snaf snaf-number)}))
                 {:acc 0
                  :snaf []})
         :snaf
         (drop-while #{0})
         (map dec->snafu)
         (apply str))))

(defn part-1 [data]
  (snaf (transduce (map unsnaf) + (str/split-lines data))))

(comment
  (= "20-1-11==0-=0112-222" (part-1 (slurp "input/2022/25.txt"))))