(ns advent-of-code-clj.y2022.d08
  (:require [clojure.string :as str]))

(defn- build-matrix [data]
  (->> data str/split-lines (mapv #(mapv parse-long (re-seq #"\d" %)))))

(defn coordinates-from-point [x y width height]
  (vector (for [xn (range (dec x) -1 -1)] [y xn])
          (for [yn (range (dec y) -1 -1)] [yn x])
          (for [xp (range (inc x) width)] [y xp])
          (for [yp (range (inc y) height)] [yp x])))

(defn part-1 [data]
  (let [m (build-matrix data)
        height (count m)
        width (count (first m))]
    (->> (for [x (range height)
               y (range width)
               :let [tree (get-in m [y x])
                     lower-tree? #(> tree (get-in m %))]]
           (some #(every? lower-tree? %) (coordinates-from-point x y width height)))
         (remove nil?)
         count)))

(defn part-2 [data]
  (let [m (build-matrix data)
        height (count m)
        width (count (first m))]
    (->> (for [x (range height)
               y (range width)
               :let [tree (get-in m [y x])
                     add-if-visible (fn [acc coords]
                                      (if (> tree (get-in m coords))
                                        (inc acc)
                                        (reduced (inc acc))))]]
           (apply * (map #(reduce add-if-visible 0 %)
                         (coordinates-from-point x y width height))))
         (apply max))))

(comment
  (part-1 (slurp "input/2022/08.txt"))
  (part-2 (slurp "input/2022/08.txt")))