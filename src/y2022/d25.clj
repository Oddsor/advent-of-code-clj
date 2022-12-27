(ns y2022.d25
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(def test-data "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

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

(assert (= 1 (unsnaf "1")))
(assert (= 2 (unsnaf "2")))
(assert (= 3 (unsnaf "1=")))
(assert (= 4 (unsnaf "1-")))
(assert (= 5 (unsnaf "10")))
(assert (= 6 (unsnaf "11")))
(assert (= 7 (unsnaf "12")))
(assert (= 8 (unsnaf "2=")))
(assert (= 9 (unsnaf "2-")))
(assert (= 10 (unsnaf "20")))
(assert (= 2022 (unsnaf "1=11-2")))
(assert (= 314159265 (unsnaf "1121-1110-1=0")))
(assert (= "1" (snaf 1)))
(assert (= "2" (snaf 2)))
(assert (= "1=" (snaf 3)))
(assert (= "10" (snaf 5)))
(assert (= "1=11-2" (snaf 2022)))
(assert (= "1121-1110-1=0" (snaf 314159265)))

(defn part-1 [data]
  (snaf (transduce (map unsnaf) + (str/split-lines data))))

(assert (= "2=-1=0" (part-1 test-data)))
(comment
  (part-1 (slurp "input/2022/25.txt")))