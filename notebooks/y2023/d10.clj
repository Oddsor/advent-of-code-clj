(ns y2023.d10
  (:require [advent-of-code-clj.utils :as u]
            [clojure.string :as str]))

(def test-data "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(defn new-coordinate [pipe prev [x y]]
  (condp = [pipe prev]
    [\J [(dec x) y]] [x (dec y)]
    [\J [x (dec y)]] [(dec x) y]
    [\L [(inc x) y]] [x (dec y)]
    [\L [x (dec y)]] [(inc x) y]
    [\- [(dec x) y]] [(inc x) y]
    [\- [(inc x) y]] [(dec x) y]
    [\| [x (dec y)]] [x (inc y)]
    [\| [x (inc y)]] [x (dec y)]
    [\7 [(dec x) y]] [x (inc y)]
    [\7 [x (inc y)]] [(dec x) y]
    [\F [x (inc y)]] [(inc x) y]
    [\F [(inc x) y]] [x (inc y)]
    nil))

(defn walk-path [matrix prev-coord current-coord]
  (lazy-seq
   (cond
     (= \S (matrix current-coord)) [current-coord]
     (#{\- \J \7 \F \| \L} (matrix current-coord))
     (cons current-coord
           (walk-path matrix
                      current-coord
                      (new-coordinate (matrix current-coord) prev-coord current-coord)))
     :else nil)))

(defn loop-path [m]
  (let [start-coord (first (keep (fn [[k v]]
                                   (when (= \S v) k)) m))
        paths (filter #(= start-coord (last %)) (keep #(walk-path m start-coord %) (apply u/adjacent-hv start-coord)))]
    (first paths)))

(defn part-1 [data]
  (-> (u/coord-map (str/split-lines data))
      loop-path
      count (/ 2)))

(= 8 (part-1 test-data))
(comment
  (= 7086 (part-1 (slurp "input/2023/d10.txt"))))

(defn part-2 [data]
  (let [coord-map (u/coord-map (str/split-lines data))
        loop-boundary (set (loop-path coord-map))
        y-range (range (inc (apply max (map second (keys coord-map)))))
        x-range (range (inc (apply max (map first (keys coord-map)))))]
    (str/join "<br/>" (map (fn [y]
                             (apply str (map (fn [x]
                                               (if (loop-boundary [x y])
                                                 (str "<font color=\"blue\">" ({\J \⌟
                                                                                \L \⌞
                                                                                \7 \⌝
                                                                                \| \|
                                                                                \- \-
                                                                                \F \⌜} (coord-map [x y])) "</font>")
                                                 (str (coord-map [x y])))) x-range))) y-range))))

#_(spit "losning.html" (part-2 test-data))
#_(spit "losning.html" (part-2 (slurp "input/2023/d10.txt")))