(ns y2023.d17
  (:require [clojure.string :as str]
            [ubergraph.alg :as ua]
            [advent-of-code-clj.utils :as u]))

(def test-data "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defn move [m n]
  (let [[x y] (:coord n)
        dir (:dir n)
        dirn (or (:dirn n) 0)]
    (filter some? [;; right
                   (let [nc [(inc x) y]]
                     (when (and (m nc)
                                (or (#{:u :d} dir)
                                    (and (= dir :r) (> 3 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :r
                               :dirn (if (not= :r dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))
                         ;; left
                   (let [nc [(dec x) y]]
                     (when (and (m nc)
                                (or (#{:u :d} dir)
                                    (and (= dir :l) (> 3 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :l
                               :dirn (if (not= :l dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))
                         ;; down
                   (let [nc [x (inc y)]]
                     (when (and (m nc)
                                (or (#{:l :r} dir)
                                    (and (= dir :d) (> 3 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :d
                               :dirn (if (not= :d dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))
                         ;; up
                   (let [nc [x (dec y)]]
                     (when (and (m nc)
                                (or (#{:l :r} dir)
                                    (and (= dir :u) (> 3 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :u
                               :dirn (if (not= :u dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))])))

(defn calc-cost 
  ([movement-algorithm data]
   (calc-cost movement-algorithm 0 data))
  ([movement-algorithm min-n data]
   (let [m (update-vals (u/coord-map (str/split-lines (str/trim data))) #(Character/getNumericValue %))
         max-y (apply max (map second (keys m)))
         max-x (apply max (map first (keys m)))]
     (:cost (ua/shortest-path (partial movement-algorithm m)
                              {:start-node {:coord [0 0]}
                               :cost-attr :weight
                               :end-node? (fn [x] (and (-> x :coord (= [max-x max-y]))
                                                       (-> x :dirn (>= min-n))))})))))

(= 102 (calc-cost move test-data))

(comment
  (= 956 (calc-cost move (slurp "input/2023/d17.txt"))))

(defn move-part-2 [m n]
  (let [[x y] (:coord n)
        dir (:dir n)
        dirn (or (:dirn n) 0)]
    (filter some? [;; right
                   (let [nc [(inc x) y]]
                     (when (and (m nc)
                                (or (and (>= dirn 4) (#{:u :d} dir))
                                    (and (= dir :r) (> 10 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :r
                               :dirn (if (not= :r dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))
                           ;; left
                   (let [nc [(dec x) y]]
                     (when (and (m nc)
                                (or (and (>= dirn 4) (#{:u :d} dir))
                                    (and (= dir :l) (> 10 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :l
                               :dirn (if (not= :l dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))
                           ;; down
                   (let [nc [x (inc y)]]
                     (when (and (m nc)
                                (or (and (>= dirn 4) (#{:l :r} dir))
                                    (and (= dir :d) (> 10 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :d
                               :dirn (if (not= :d dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))
                           ;; up
                   (let [nc [x (dec y)]]
                     (when (and (m nc)
                                (or (and (>= dirn 4) (#{:l :r} dir))
                                    (and (= dir :u) (> 10 dirn))
                                    (nil? dir)))
                       {:dest {:coord nc
                               :dir :u
                               :dirn (if (not= :u dir)
                                       1
                                       (inc dirn))} :weight (m nc)}))])))

(= 94 (calc-cost move-part-2 4 test-data))

(def test-data-2 "111111111111
999999999991
999999999991
999999999991
999999999991")

(= 71 (calc-cost move-part-2 4 test-data-2))

(comment
  (= 1104 (calc-cost move-part-2 4 (slurp "input/2023/d17.txt"))))