(ns y2024.d14
  (:require
   [clojure.core.matrix :as mx]
   [advent-of-code-clj.input :as input]))

(def test-data "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn robots [input]
  (eduction
   (map parse-long)
   (partitionv-all 2)
   (map (fn [x] (vec (reverse x))))
   (partitionv-all 2)
   (re-seq #"-?\d+" input)))

(robots test-data)

(defn move-robot [times position velocity]
  (mx/add position (mx/scale velocity times)))

(defn bound-robot [dimensions position]
  (mapv #(mod %1 %2) position dimensions))

(->> (apply move-robot 100 (first (robots test-data)))
     (bound-robot [7 11]))

(defn quadrants [[dimy dimx]]
  [; quad1 
   (set (for [y (range (int (Math/floor (/ dimy 2))))
              x (range (int (Math/floor (/ dimx 2))))]
          [y x]))
   ; quad2
   (set (for [y (range (int (Math/floor (/ dimy 2))))
              x (range (int (Math/ceil (/ dimx 2))) dimx)]
          [y x]))
   ; quad3
   (set (for [y (range (int (Math/ceil (/ dimy 2))) dimy)
              x (range (int (Math/floor (/ dimx 2))))]
          [y x]))
   ; quad4
   (set (for [y (range (int (Math/ceil (/ dimy 2))) dimy)
              x (range (int (Math/ceil (/ dimx 2))) dimx)]
          [y x]))])

(defn print-board [[y x] robots]
  (mx/pm (mx/emap-indexed
          (fn [index _]
            (count (filter (set [(vec index)])
                           robots)))
          (mx/new-matrix y x))))

(defn part-1 [dims input]
  (let [robot-positions (->> (robots input)
                             (map #(apply move-robot 100 %))
                             (map #(bound-robot dims %)))
        quads (quadrants dims)]
    #_(print-board dims robot-positions)
    (transduce (map (fn [q]
                      (count (filter q robot-positions))))
               *
               quads)))

(part-1 [7 11] test-data)

(part-1 [103 101] (input/get-input 2024 14))
