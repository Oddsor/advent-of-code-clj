^:kindly/hide-code
(ns y2025.d09
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.math.combinatorics :as combinatorics]
    [clojure.string :as str]))

; # 2025, dag 9

; ## Del 1

; Dagens oppgave starter tilsynelatende ganske enkelt, vi skal finne det største mulige rektangelet 
; mellom to av X mulige punkter. For eksempel testdataene:

(def test-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defrecord Point [^int x ^int y])

(defn points [input]
  (->> input
       (re-seq #"(\d+),(\d+)")
       (mapv (fn [[_ x y]]
               (->Point (Integer/parseInt x) (Integer/parseInt y))))))

(points test-input)

; I test-inputten skal største rektangel være mellom 2,5 og 11,1. Her er det
; bare å kalkulere alle areal og sortere:

(defn area [^Point a ^Point b]
  (* (inc (abs (- (.y b) (.y a))))
     (inc (abs (- (.x b) (.x a))))))

(defn with-area [points]
  (->> (combinatorics/combinations points 2)
       (mapv (fn [[a b]]
               [(area a b) a b]))))

(defn part-1 [input]
  (->> input
       points
       with-area
       (sort-by first >)
       first))

(part-1 test-input)

(part-1 (input/get-input 2025 9))

; ## Del 2

; I del 2 skal vi finne største mulige region i et polygon, representert av
; input-dataene våre.

; Vi kan visualisere polygonet via svg:

(defn make-svg [input]
  (let [points (points input)
        min-x (apply min (map :x points))
        max-x (apply max (map :x points))
        min-y (apply min (map :y points))
        max-y (apply max (map :y points))]
    [:svg {:viewBox (str/join " " [min-x min-y max-x max-y])}
     [:polygon {:points (str/replace input "\n" " ")}]]))

^:kind/hiccup (make-svg test-input)
^:kind/hiccup (make-svg (input/get-input 2025 9))

(defn intersection
  "Reimplementert algoritme fra https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment"
  [[a1 a2] [b1 b2]]
  (let [{x1 :x y1 :y} a1
        {x2 :x y2 :y} a2
        {x3 :x y3 :y} b1
        {x4 :x y4 :y} b2
        denom (- (* (- x1 x2) (- y3 y4))
                 (* (- y1 y2) (- x3 x4)))
        t (try (/ (- (* (- x1 x3) (- y3 y4))
                     (* (- y1 y3) (- x3 x4)))
                  denom)
               (catch ArithmeticException _ 1000))
        u (try (- (/ (- (* (- x1 x2) (- y1 y3))
                        (* (- y1 y2) (- x1 x3)))
                     denom))
               (catch ArithmeticException _ 1000))]
    (when (and (<= 0 t 1) (<= 0 u 1))
      {:x (+ x1 (* (- x2 x1) t))
       :y (+ y1 (* (- y2 y1) t))})))

(defn polygon-edges [points]
  (->> (partitionv 2 1 points)
       (cons [(last points) (first points)])
       (map (fn [[a b]]
              (let [[min-x max-x] ((juxt min max) (:x a) (:x b))
                    [min-y max-y] ((juxt min max) (:y a) (:y b))]
                [min-x max-x min-y max-y])))))

(defn intersects-rectangle? [p-min-x p-max-x p-min-y p-max-y rectangle-min-x rectangle-max-x rectangle-min-y rectangle-max-y]
  (cond
    ; Sjekk vertikal akse
    (and (= p-min-x p-max-x)
         (< rectangle-min-x p-min-x rectangle-max-x))
    (not (or (>= p-min-y rectangle-max-y)
             (<= p-max-y rectangle-min-y)))
    ; Sjekk horisontal akse
    (and (= p-min-y p-max-y) (< rectangle-min-y p-min-y rectangle-max-y))
    (not (or (>= p-min-x rectangle-max-x)
             (<= p-max-x rectangle-min-x)))
    :else nil))

(defn valid-fast? [polygon solution]
  (let [[_area p1 p2] solution
        r-min-x (min (:x p1) (:x p2))
        r-max-x (max (:x p1) (:x p2))
        r-min-y (min (:y p1) (:y p2))
        r-max-y (max (:y p1) (:y p2))]
    (not-any? (fn [[a b c d]]
                (intersects-rectangle? a b c d r-min-x r-max-x r-min-y r-max-y)) 
              polygon)))

(defn part-2-faster [input]
  (let [points (points input)
        polygon (polygon-edges points)
        solutions (->> points
                       with-area
                       (sort-by first >))]
    (first (filter #(valid-fast? polygon %) solutions))))

(part-2-faster test-input)

(delay (part-2-faster (input/get-input 2025 9)))

^:kind/hiccup
(delay
  (let [input (input/get-input 2025 9)
        [_ r1 r2] (part-2-faster (input/get-input 2025 9))
        r-min-x (min (:x r1) (:x r2))
        r-min-y (min (:y r1) (:y r2))
        r-width (abs (- (:x r1) (:x r2)))
        r-height (abs (- (:y r1) (:y r2)))
        points (points input)
        min-x (apply min (map :x points))
        max-x (apply max (map :x points))
        min-y (apply min (map :y points))
        max-y (apply max (map :y points))]
    [:svg {:viewBox (str/join " " [min-x min-y max-x max-y])}
     [:polygon {:points (str/replace input "\n" " ")
                :fill "gray"}]
     [:rect {:x r-min-x :y r-min-y 
             :width r-width :height r-height
             :fill "red"}]]))
