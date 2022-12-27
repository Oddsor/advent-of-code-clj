(ns y2022.d24
  (:require [clojure.string :as str]))

(defn parse [data]
  (let [m (mapv vec (str/split-lines data))
        max-y (dec (count m))
        max-x (dec (count (first m)))]
    {:max-y max-y
     :max-x max-x
     :blizzards (for [y (range 1 (inc max-y))
                      x (range 1 (inc max-x))
                      :let [at-coord (get-in m [y x])]
                      :when (#{\< \> \^ \v} at-coord)]
                  [({\< [-1 0]
                     \> [1 0]
                     \^ [0 -1]
                     \v [0 1]} at-coord) x y])}))

(defn move-blizzard [max-x max-y [d x y]]
  (let [[dx dy] d
        nx (+ x dx) ny (+ y dy)]
    [d
     (cond
       (= 0 nx) (dec max-x)
       (= max-x nx) 1
       :else nx)
     (cond
       (= 0 ny) (dec max-y)
       (= max-y ny) 1
       :else ny)]))

(defn blizzard-seq [max-x max-y xs]
  (map (fn [xs] (map (fn [[_ x y]] [x y]) xs))
       (iterate #(map (partial move-blizzard max-x max-y) %) xs)))

(defn travel-to [from to max-x max-y blizzards]
  (loop [n 1
         positions [from]]
    (let [bc (set (nth blizzards n))
          npositions (->> positions
                          (mapcat (fn [[x y :as coord]]
                                    (->> [[(dec x) y] [(inc x) y] coord
                                          [x (dec y)] [x (inc y)]]
                                         (filter (fn [[x y]]
                                                   (or (= [x y] [1 0])
                                                       (= [x y] [(dec max-x) max-y])
                                                       (and (< 0 x max-x) (< 0 y max-y)))))
                                         (remove bc))))
                          distinct)]
      (if (some #{to} npositions)
        n
        (recur (inc n) npositions)))))

(defn part-1 [data]
  (let [{:keys [max-y max-x blizzards]} (parse data)
        end-position [(dec max-x) max-y]]
    (travel-to [1 0] end-position max-x max-y (blizzard-seq max-x max-y blizzards))))

(defn part-2 [data]
  (let [{:keys [max-y max-x blizzards]} (parse data)
        end-position [(dec max-x) max-y]
        blizzards (blizzard-seq max-x max-y blizzards)
        round-1 (travel-to [1 0] end-position max-x max-y (drop 1 blizzards))
        round-2 (travel-to end-position [1 0] max-x max-y (drop round-1 blizzards))
        round-3 (travel-to [1 0] end-position max-x max-y (drop (+ round-1 round-2) blizzards))]
    (+ round-1 round-2 round-3)))

(comment
  (= 314 (part-1 (slurp "input/2022/24.txt")))
  (= 896 (part-2 (slurp "input/2022/24.txt"))))