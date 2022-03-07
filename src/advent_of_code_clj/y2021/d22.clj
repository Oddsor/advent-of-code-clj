(ns advent-of-code-clj.y2021.d22
  (:require [clojure.string :as str]))

(defn parse-line [t]
  (let [[x1 x2 y1 y2 z1 z2] (map read-string (re-seq #"[-\d]+" t))
        enable? ({"on" true "off" false} (first (str/split t #" ")))]
    {:enable? enable?
     :x1 x1 :x2 x2 :y1 y1 :y2 y2 :z1 z1 :z2 z2}))

(defn parse [t]
  (map parse-line (str/split-lines t)))

(def test-data (parse "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"))

(assert (= 39
           (->> (reduce (fn [acc {:keys [enable? x1 x2 y1 y2 z1 z2]}]
                          (->> (for [x (range x1 (inc x2))
                                     :when (> 51 x -51)
                                     y (range y1 (inc y2))
                                     :when (> 51 y -51)
                                     z (range z1 (inc z2))
                                     :when (> 51 z -51)]
                                 [[x y z] enable?])
                               (into acc))) {} test-data)
                vals
                (filter true?)
                count)))

(comment
  ;; Part 1
  (->> (parse (slurp "input/y2021/22.txt"))
       (reduce (fn [acc {:keys [enable? x1 x2 y1 y2 z1 z2]}]
                 (->> (for [x (range x1 (inc x2))
                            :when (> 51 x -51)
                            y (range y1 (inc y2))
                            :when (> 51 y -51)
                            z (range z1 (inc z2))
                            :when (> 51 z -51)]
                        [[x y z] enable?])
                      (into acc))) {})
       vals (filter true?) count)
  ;; Part 2
  ;; too slow
  (->> (parse (slurp "input/y2021/22.txt"))
       (reduce (fn [acc {:keys [enable? x1 x2 y1 y2 z1 z2]}]
                 (->> (for [x (range x1 (inc x2))
                            y (range y1 (inc y2))
                            z (range z1 (inc z2))]
                        [[x y z] enable?])
                      (into acc))) {})
       vals (filter true?) count)
  ;;
  )