(ns y2022.d17
  (:require [clojure.string :as str]))

(def test-data ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defn jet-seq [data]
  (cycle (keep {\> 1 \< -1} (seq data))))
(def rock-cycle (cycle [:- :+ :J :| :.]))

(defn get-rock [rock-type current-height]
  (let [bottom-y (+ current-height 3)]
    (case rock-type
      :- [[2 bottom-y] [3 bottom-y] [4 bottom-y] [5 bottom-y]]
      :+ [[3 bottom-y] [2 (inc bottom-y)] [3 (inc bottom-y)] [4 (inc bottom-y)] [3 (+ bottom-y 2)]]
      :J [[2 bottom-y] [3 bottom-y] [4 bottom-y] [4 (inc bottom-y)] [4 (+ bottom-y 2)]]
      :| [[2 bottom-y] [2 (inc bottom-y)] [2 (+ bottom-y 2)] [2 (+ bottom-y 3)]]
      :. [[2 bottom-y] [3 bottom-y] [2 (inc bottom-y)] [3 (inc bottom-y)]])))

(defn move-rock [rock direction visited]
  (let [pushed (mapv (fn [[x y]]
                       [(+ x direction) y]) rock)
        ns (if (every? (fn [[x y]]
                         (and (<= 0 x 6)
                              (not (visited [x y])))) pushed)
             pushed
             rock)
        fall (mapv (fn [[x y]]
                     [x (dec y)]) ns)]
    (if (every? (fn [[x y]]
                  (and (<= 0 y)
                       (not (visited [x y])))) fall)
      [:falling fall]
      [:resting ns])))

(defn get-state-after [num-rocks data]
  (loop [n 100000000
         [jet & jets] (jet-seq data)
         resting-rocks 0
         current-height 0
         rock (get-rock (first rock-cycle) 0)
         rock-cycle (next rock-cycle)
         visited #{}]
    (if (or (zero? n) (= resting-rocks num-rocks))
      {:rested visited :next rock :height (inc current-height)}
      (let [[state moved-rock] (move-rock rock jet visited)
            rock-stuck? (= :resting state)
            new-height (if rock-stuck?
                         (max current-height (apply max (map second moved-rock)))
                         current-height)]
        (recur (dec n)
               jets
               (if rock-stuck? (inc resting-rocks) resting-rocks)
               new-height
               (if rock-stuck?
                 (get-rock (first rock-cycle) (inc new-height))
                 moved-rock)
               (if rock-stuck? (next rock-cycle) rock-cycle)
               (if rock-stuck? (into visited moved-rock) visited))))))

(defn part-1 [num-rocks data]
  (:height (get-state-after num-rocks data)))

(defn print-board [nodes]
  (let [max-y (apply max (map second nodes))]
    (println (str/join "\n" (reverse (map (fn [y]
                                            (apply str (map (fn [x] (if (nodes [x y]) \# \.)) (range 7))))
                                          (range (inc max-y))))))))

(assert (= 3068 (part-1 2022 test-data)))

(comment
  (let [{:keys [rested next]} (get-state-after 10 test-data)]
    (print-board (into rested next)))
  ;; Impossible with naive solution
  (= 1514285714288 (part-1 1000000000000 test-data))
  (part-1 2022 (slurp "input/2022/17.txt")))