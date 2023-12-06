(ns y2023.d06
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def test-data "Time:      7  15   30
Distance:  9  40  200")

;; Lifted from this answer at stackoverflow: https://stackoverflow.com/a/60642870
(defn quadraticRoots [a b c]
  (let [discriminant (math/sqrt (- (math/pow b 2)
                                   (* 4 a c)))
        root-1       (/ (+ (- b) discriminant)
                        (* 2 a))
        root-2       (/ (- (- b) discriminant)
                        (* 2 a))]
    [root-1 root-2]))

(defn winning-scenarios [race-time record-distance]
  (let [[fst lst] (quadraticRoots -1 race-time (- record-distance))
        fstceil (int (math/ceil fst))
        lstfloor (int (math/floor lst))]
    (- (cond-> lstfloor (== lstfloor lst) dec)
       (cond-> fstceil (== fstceil fst) inc)
       -1)))

(defn part-1 [data]
  (->> data
       str/split-lines
       (mapv (partial re-seq #"\d+"))
       (mapv (partial mapv parse-long))
       (apply zipmap)
       (reduce-kv (fn [acc race record]
                    (* acc (winning-scenarios race record))) 1)))

(= 288 (part-1 test-data))
(comment
  (= 512295 (part-1 (slurp "input/2023/d06.txt"))))

(defn part-2 [data]
  (->> (str/split-lines data)
       (map (partial re-seq #"\d+"))
       (map (comp parse-long str/join))
       (apply winning-scenarios)))

(= 71503 (part-2 test-data))
(comment
  (= 36530883 (part-2 (slurp "input/2023/d06.txt"))))