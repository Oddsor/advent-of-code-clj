(ns advent-of-code-clj.y2022.d08
  (:require [clojure.string :as str]))

(defn- build-matrix [data]
  (->> data str/split-lines vec (mapv #(vec (map parse-long (re-seq #"\d" %))))))
(defn- visible-trees-along-edges [m]
  (+ (* (count m) 2)
     (* (- (count (first m)) 2) 2)))

(defn part-1 [data]
  (let [m (build-matrix data)
        height (count m)
        width (count (first m))]
    (->> (for [x (range 1 (dec height))
               y (range 1 (dec width))
               :let [tree (-> m (nth y) (nth x))]]
           (or (every? #(> tree (get-in m %))
                       (for [xn (range (dec x) -1 -1)]
                         [y xn]))
               (every? #(> tree (get-in m %))
                       (for [yn (range (dec y) -1 -1)]
                         [yn x]))
               (every? #(> tree (get-in m %))
                       (for [xp (range (inc x) width)]
                         [y xp]))
               (every? #(> tree (get-in m %))
                       (for [yp (range (inc y) height)]
                         [yp x]))))
         (filter true?)
         count
         (+ (visible-trees-along-edges m)))))

(defn part-2 [data]
  (let [m (build-matrix data)
        height (count m)
        width (count (first m))]
    (->> (for [x (range height)
               y (range width)
               :let [tree (-> m (nth y) (nth x))
                     add-if-visible (fn [acc coords]
                                      (if (> tree (get-in m coords))
                                        (inc acc)
                                        (reduced (inc acc))))]]
           (* (reduce add-if-visible 0
                      (for [xn (range (dec x) -1 -1)]
                        [y xn]))
              (reduce add-if-visible 0
                      (for [yn (range (dec y) -1 -1)]
                        [yn x]))
              (reduce add-if-visible 0
                      (for [xp (range (inc x) width)]
                        [y xp]))
              (reduce add-if-visible 0
                      (for [yp (range (inc y) height)]
                        [yp x]))))
         (apply max))))

(comment
  (part-1 (slurp "input/2022/08.txt"))
  (part-2 (slurp "input/2022/08.txt")))