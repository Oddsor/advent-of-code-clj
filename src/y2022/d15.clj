(ns y2022.d15)

(def test-data "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse [data]
  (->> data
       (re-seq #"-?\d+")
       (map parse-long)
       (partition 4)
       (map (comp (partial mapv vec) (partial partition 2)))
       (into {})))

(defn distance [[a b] [c d]]
  (+ (- (max a c) (min a c))
     (- (max b d) (min b d))))

(defn part-1 [y-row data]
  (let [d (parse data)
        beacon->distance (into {} (map (fn [[k v]] [k (distance k v)])) d)
        beacon-coords (set (vals d))
        min-x (apply min (map (fn [[[x _] v]] (- x v)) beacon->distance))
        max-x (apply max (map (fn [[[x _] v]] (+ x v)) beacon->distance))]
    (count (remove beacon-coords
                   (keep (fn [x]
                           (when (some (fn [[sensor distance-to-nearest]]
                                         (>= distance-to-nearest (distance [x y-row] sensor)))
                                       beacon->distance)
                             [x y-row])) (range min-x (inc max-x)))))))

(assert (= 26 (part-1 10 test-data)))
(defn perimeter [[x y] radius]
  (mapcat identity
          (for [n (range (inc (* 2 (inc radius))))
                :let [i (if (> n (inc radius)) (- (inc radius) (- n (inc radius))) n)]]
            [[(+ (- x (+ radius 1)) n) (+ y i)] [(+ (- x (+ radius 1)) n) (- y i)]])))

;; Was stuck but got spoiled on the idea to only test the perimeters
(defn part-2 [size data]
  (let [d (parse data)
        beacon-coords (set (vals d))
        beacon->radius (into {} (map (fn [[k v]] [k (distance k v)])) d)
        [x y] (->> beacon->radius
                   (reduce-kv (fn [visited sensor radius]
                                (let [pnodes (perimeter sensor radius)]
                                  (if-let [found (some (fn [[x y]]
                                                         (when (and (<= 0 x size) (<= 0 y size)
                                                                    (not (beacon-coords [x y]))
                                                                    (not-any? (fn [[sensor distance-to-nearest]]
                                                                                (>= distance-to-nearest (distance [x y] sensor))) beacon->radius))
                                                           [x y]))
                                                       (remove visited pnodes))]
                                    (reduced found)
                                    (into visited pnodes))))
                              #{}))]
    (+ (* 4000000 x) y)))

(assert (= 56000011 (part-2 20 test-data)))

(comment
  (= 5144286 (part-1 2000000 (slurp "input/2022/15.txt")))
  (= 10229191267339 (part-2 4000000 (slurp "input/2022/15.txt"))))