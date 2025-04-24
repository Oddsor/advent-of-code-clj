(ns advent-of-code-clj.y2021.d09-map
  (:require [clojure.core.reducers :as r]
            [clojure.string :as str]))

(defn parse-nums [idy text]
  (apply merge
         (map-indexed (fn [idx t]
                        {[idx idy] (read-string t)}) (re-seq #"\d" text))))
(defn parse-lines [text]
  (apply merge
         (map-indexed (partial parse-nums) (str/split-lines text))))

(def test-data (parse-lines "2199943210
3987894921
9856789892
8767896789
9899965678"))

(defn adjacent [x y]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn find-minima [data]
  (->> data
       (r/filter (fn [point value]
                   (< value
                      (apply min (keep data (apply adjacent point))))))
       (r/fold (r/monoid merge hash-map) assoc)))

(r/fold (r/monoid merge hash-map) assoc (r/filter (fn [_k v] (odd? v)) {[1 2] 1 :b 2 :c 3 :d 4}))
(into [] (r/filter odd? [1 2 3 4]))

(defn higher-adjacent-points [data point]
  (->> (apply adjacent point)
       (select-keys data)
       (filter #(> 9 (val %) (data point)))
       keys))

(defn climb [data points]
  (lazy-seq
    (if-let [xs (->> points
                     (r/mapcat (partial higher-adjacent-points data))
                     r/foldcat
                     seq)]
      (cons points (climb data xs))
      [points])))

(comment (climb test-data [[9 0]]))
;; ([[9 0]] 
;;  ([8 0] [9 1])
;;  ([7 0] [8 1] [8 1] [9 2])
;;  ([6 0])
;;  ([5 0] [6 1]))
;; 2199943210      0000054321 
;; 3987894921      0000005032
;; 9856789892  ->  0000000003
;; 8767896789      0000000000
;; 9899965678      0000000000

(defn area [plot-groups]
  (count (set (mapcat identity plot-groups))))

(assert (= 15 (apply + (map (comp inc val) (find-minima test-data)))))

(assert (= 1134 (->> test-data
                     find-minima
                     keys
                     (map (fn [xs]
                            (->> xs
                                 vector
                                 (climb test-data)
                                 area)))
                     sort
                     (take-last 3)
                     (apply *))))

(comment
  (time (let [data (parse-lines (slurp "input/y2021/09.txt"))
              minima (find-minima data)]
          [(->> minima
                (map (comp inc val))
                (apply +)
                (= 444))
           (->> minima
                keys
                (pmap (fn [plot]
                        (area (climb data [plot]))))
                (sort >)
                (take 3)
                (apply *)
                (= 1168440))])))
