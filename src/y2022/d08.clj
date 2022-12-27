(ns y2022.d08
  (:require [clojure.string :as str]))

(defn- build-matrix [data]
  (->> data str/split-lines (mapv #(mapv parse-long (re-seq #"\d" %)))))

(defn coordinates-from-point [x y width height]
  (vector (for [xn (range (dec x) -1 -1)] [y xn])
          (for [yn (range (dec y) -1 -1)] [yn x])
          (for [xp (range (inc x) width)] [y xp])
          (for [yp (range (inc y) height)] [yp x])))

(defn apply-to-nearby-trees [fun m]
  (let [height (count m)
        width (count (first m))]
    (for [x (range height)
          y (range width)]
      (fun (get-in m [y x]) (mapv (partial mapv #(get-in m %)) (coordinates-from-point x y width height))))))

(defn part-1 [data]
  (->> (build-matrix data)
       (apply-to-nearby-trees
        (fn [tree coords]
          (let [lower-tree? #(> tree %)]
            (some #(every? lower-tree? %) coords))))
       (remove nil?)
       count))

(defn part-2 [data]
  (->> (build-matrix data)
       (apply-to-nearby-trees
        (fn [tree coords]
          (let [add-if-visible (fn [acc other-tree]
                                 ((if (> tree other-tree)
                                    identity
                                    reduced) (inc acc)))]
            (apply * (map #(reduce add-if-visible 0 %) coords)))))
       (apply max)))

(comment
  (= 1776 (part-1 (slurp "input/2022/08.txt")))
  (= 234416 (part-2 (slurp "input/2022/08.txt"))))