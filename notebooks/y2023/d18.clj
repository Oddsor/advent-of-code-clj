(ns y2023.d18
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(def test-data "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(def dir->vector
  {"R" [1 0]
   "L" [-1 0]
   "U" [0 -1]
   "D" [0 1]})

(defn p-line [text]
  (let [[dir n color] (str/split text #"\s+")]
    {:dir dir
     :n (parse-long n)
     :color (subs color 1 (dec (count color)))}))

(defn instructions->trench [instructions]
  (reduce
   (fn [{:keys [pos dug]} {:keys [dir n color]}]
     (let [[dx dy :as dirv] (dir->vector dir)
           npos (mapv + pos (mapv * dirv [n n]))
           ndug (map (fn [np]
                       {np color}) (take n (iterate (fn [[x y]]
                                                      [(+ x dx) (+ y dy)]) pos)))]
       {:pos npos
        :dug (into dug ndug)}))
   {:dug {}
    :pos [0 0]} instructions))

(defn instructions->edges [instructions]
  (reductions
   (fn [pos {:keys [dir n]}]
     (let [dirv (dir->vector dir)
           npos (mapv + pos (mapv * dirv [n n]))]
       npos))
   [0 0] instructions))

(defn polygon-area [coordinates]
  (/ (transduce (map (fn [[[ax ay] [bx by]]]
                       (- (* ax by) (* bx ay))))
                +
                (cons [(last coordinates) (first coordinates)] (partition 2 1 coordinates)))
     2))

(defn vector-distance [[ax ay] [bx by]]
  (math/sqrt (+ (math/pow (- bx ax) 2) (math/pow (- by ay) 2))))

(defn polygon-circumference [coordinates]
  (transduce (map (partial apply vector-distance))
             +
             (cons [(last coordinates) (first coordinates)] (partition 2 1 coordinates))))

(defn part-1 [data]
  (let [edges (vec (butlast (instructions->edges
                             (map p-line (str/split-lines data)))))]
    (int (+ (inc (/ (polygon-circumference edges) 2))
            (polygon-area edges)))))

(= 62 (part-1 test-data))
(comment
  (= 40745 (part-1 (slurp "input/2023/d18.txt"))))

(defn hex->instruction [hex]
  (let [dir (case (last hex)
              \0 "R"
              \1 "D"
              \2 "L"
              \3 "U")
        n (subs hex 1 (dec (count hex)))]
    {:dir dir
     :n (Integer/parseInt n 16)}))

(defn part-2 [data]
  (let [instructions (map (fn [line]
                            (->> (re-seq #"\((.*)\)" line)
                                 first
                                 second
                                 hex->instruction))
                          (str/split-lines data))
        edges (vec (instructions->edges instructions))]
    (+ (+ (/ (polygon-circumference edges) 2) 1) (polygon-area edges))))

(== 952408144115
    (part-2 test-data))

(comment
  (== 90111113594927
      (part-2 (slurp "input/2023/d18.txt"))))