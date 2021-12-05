(ns advent-of-code-clj.y2021.dag5
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]))

(def test-data "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn plotify
  "\"7,0 -> 7,4\" => ((7 0) (7 4))"
  [plot-str]
  (->> plot-str
       (re-seq #"\d+")
       (map read-string)
       (partition 2)))

(defn parse [text]
  (map plotify (str/split-lines text)))

(defn apply-plot [board [[x1 y1 :as a] [x2 y2 :as b]]]
  #_(update board x1 (fn [row]
                       (update row y1 (fnil inc 0))))
  (let [updated-board (s/transform [x1 y1 (s/nil->val 0)] inc board)]
    (if (= a b)
      updated-board
      (recur updated-board [[(+ x1 (compare x2 x1))
                             (+ y1 (compare y2 y1))] b]))))

(defn intersecting-points [xs-of-xs]
  (count (filter #(> % 1)
                 (mapcat #(remove nil? %) xs-of-xs))))

(defn only-horizontal-or-vertical? [[a b]]
  (some zero? (map - a b)))

(defn init-grid [size]
  (vec (repeat size (vec (repeat size nil)))))

(assert (= 5
           (intersecting-points
            (reduce apply-plot
                    (init-grid 10)
                    (filter only-horizontal-or-vertical?
                            (parse test-data))))))

(assert (= 12
           (intersecting-points
            (reduce apply-plot
                    (init-grid 10)
                    (parse test-data)))))

(comment
  ;; Part 1
  (= 6225
     (intersecting-points
      (reduce apply-plot
              (init-grid 1000)
              (filter (fn [[[x1 y1] [x2 y2]]]
                        (or (= x1 x2)
                            (= y1 y2)))
                      (parse (slurp "input/y2021/day6-input.txt"))))))

  ;; Part 2
  (= 22116
     (intersecting-points
      (reduce apply-plot
              (init-grid 1000)
              (parse (slurp "input/y2021/day6-input.txt")))))
  ;;
  )

;; Second variant instead generates points and counts all repetitions

(defn plots [[x1 y1] [x2 y2 :as b]]
  (loop [p []
         x1 x1
         y1 y1]
    (let [np (conj p [x1 y1])]
      (if (= [x1 y1] b)
        np
        (recur np
               (+ x1 (compare x2 x1))
               (+ y1 (compare y2 y1)))))))

(assert (= 5
           (->> test-data
                parse
                (filter only-horizontal-or-vertical?)
                (mapcat (partial apply plots))
                frequencies
                (filter #(> (val %) 1))
                count)))

(assert (= 12
           (->> test-data
                parse
                (mapcat (partial apply plots))
                frequencies
                (filter #(> (val %) 1))
                count)))