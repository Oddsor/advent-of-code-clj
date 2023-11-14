^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns y2022.d22
  (:require [advent-of-code-clj.utils :refer [emap split-newline text->matrix]]
            [nextjournal.clerk :as clerk]))

;; # Year 2022, day 22

;; ## Part 1

;; In this exercise, we are given a map-layout and a series of operations. The dots (.) indicate an open space,
;; while hash-symbols (#) indicate a wall. When the character moves out of bounds (whitespace), they will "wrap around"
;; to the first available space on the other end of the map. So for example, going north from the top will make the
;; character pop up on the southern side of the map.

;; The last part of the dataset indicates the movement of the character. Numbers represent the number of steps forward,
;; while L/R represents turning to the left or right.

;; The character starts at the first available space at the top of the map, facing right.

;; The test dataset looks like this:

(def test-data "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse [data]
  (let [[map directions] (split-newline data)
        mapvec (->> map text->matrix (emap {\space :_ \# :# \. :.}))
        operations (->> directions
                        (re-seq #"[LR]|\d+")
                        (mapv (fn [x]
                           (if-let [xn (parse-long x)]
                             xn
                             ({"R" :R "L" :L} x)))))]
    [{:pos [(first (for [i (range (count (first mapvec)))
                         :when (= :. (nth (first mapvec) i))]
                     i)) 0]
      :dir :R}
     mapvec
     operations]))

(parse test-data)

^{:nextjournal.clerk/visibility {:result :hide}}
(defn wrap-around [m [x y] d]
  (cond
    (#{:L :R} d)
    (let [row (get m y)
          relevant-indexes (for [i (range (count row))
                                 :when (not= :_ (get row i :_))]
                             i)]
      [(if (= :L d) (last relevant-indexes) (first relevant-indexes)) y])
    (#{:U :D} d)
    (let [relevant-indexes (for [i (range (count m))
                                 :when (not= :_ (get-in m [i x] :_))]
                             i)]
      [x (if (= :U d) (last relevant-indexes) (first relevant-indexes))])))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn next-coord [m [x y] d]
  (let [nx (cond-> x
             (= :R d) inc
             (= :L d) dec)
        ny (cond-> y
             (= :D d) inc
             (= :U d) dec)
        p (get-in m [ny nx] nil)]
    (cond
      (= p :.) [nx ny]
      (= p :#) [x y]
      (or (nil? p) (= :_ p))
      (let [[px py] (wrap-around m [x y] d)]
        (if (= :# (get-in m [py px]))
          [x y]
          [px py])))))

(def new-dir {[:R :R] :D
              [:R :L] :U
              [:L :R] :U
              [:L :L] :D
              [:U :L] :L
              [:U :R] :R
              [:D :R] :L
              [:D :L] :R})

^{:nextjournal.clerk/visibility {:result :hide}}
(defn calculate [{[x y] :pos d :dir}]
  (+ (* 1000 (inc y))
     (* 4 (inc x))
     ({:R 0 :D 1 :L 2 :U 3} d)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn walk-map [state m ops]
  (reductions (fn [{p :pos d :dir :as s} op]
                (if (number? op)
                  (loop [p p
                         n op]
                    (let [np (next-coord m p d)]
                      (if (or (= 1 n) (= np p))
                        (assoc s :pos np)
                        (recur np (dec n)))))
                  (assoc s :dir (new-dir [d op])))) state ops))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [[state m ops] (parse data)]
    (calculate (last (walk-map state m ops)))))

(= 6032 (part-1 test-data))

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 123046 (part-1 (slurp "input/2022/22.txt"))))