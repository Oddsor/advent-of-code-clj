(ns advent-of-code-clj.y2021.d07)
(defn parse [text]
  (map read-string (re-seq #"\d+" text)))
(def test-data (parse "16,1,2,0,4,2,7,1,2,14"))

(defn triangle [n]
  ;; With a reduce-strategy, performance degrades so much
  ;; that the fact-function needs to be memoized
  ;; (calc-min-fuel (memoize fact) data))
  #_(reduce + (range (inc n)))
  (/ (* n (inc n)) 2))

(defn distance [a b]
  (Math/abs (- a b)))

(defn calc-min-f [f data]
  (->> (for [i (range (count data))
             x data]
         (f i x))
       (partition (count data))
       (map (partial apply +))
       (apply min)))

(assert (= 37 (calc-min-f distance test-data)))
(assert (= 168 (calc-min-f (comp triangle distance) test-data)))

;; Take 2 with MATH

(defn mean [data]
  (nth (sort data) (/ (count data) 2)))

(defn avg [data]
  (float (/ (apply + data) (count data))))

(defn calc-distance-with-f [f n data]
  (apply + (map (partial f n) data)))

(defn find-min-distance [data]
  ;; For part 2, optimal index will be within 0.5 of the average (floor or ceiling)
  ;; https://www.reddit.com/r/adventofcode/comments/rawxad/2021_day_7_part_2_i_wrote_a_paper_on_todays/
  (->> (avg data)
       ((juxt #(Math/floor %) #(Math/ceil %)))
       (map #(calc-distance-with-f (comp triangle distance) (int %) data))
       (apply min)))

(assert (= 37 (calc-distance-with-f distance (mean test-data) test-data)))

(assert (= 168 (find-min-distance test-data)))

(comment
  (def data (parse (slurp "input/y2021/07.txt")))

  (= 328262 (calc-min-f distance data))
  (= 90040997 (calc-min-f (comp triangle distance) data))

  (time (= 328262 (apply + (map (partial distance (mean data)) data))))
  (time (= 90040997 (find-min-distance data)))
  ;;
  )