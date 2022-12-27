(ns y2022.d22
  (:require [advent-of-code-clj.utils :refer [emap split-newline text->matrix]]))

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

(defn parse [data]
  (let [[map directions] (split-newline data)
        mapvec (->> map text->matrix (emap {\space :_ \# :# \. :.}))
        operations (mapv (fn [x]
                           (if-let [xn (parse-long x)]
                             xn
                             ({"R" :R "L" :L} x)))
                         (re-seq #"[LR]|\d+" directions))]
    [{:pos [(first (for [i (range (count (first mapvec)))
                         :when (= :. (nth (first mapvec) i))]
                     i)) 0]
      :dir :R}
     mapvec
     operations]))

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

(defn calculate [{[x y] :pos d :dir}]
  (+ (* 1000 (inc y))
     (* 4 (inc x))
     ({:R 0 :D 1 :L 2 :U 3} d)))

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

(defn part-1 [data]
  (let [[state m ops] (parse data)]
    (calculate (last (walk-map state m ops)))))

(assert (= 6032 (part-1 test-data)))

(comment (= 123046 (part-1 (slurp "input/2022/22.txt")))
         (apply walk-map (parse test-data))
         (apply walk-map (parse (slurp "input/2022/22.txt"))))