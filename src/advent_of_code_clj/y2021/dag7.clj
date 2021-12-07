(ns advent-of-code-clj.y2021.dag7)
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

(defn avg-int [data]
  (Math/round (float (/ (apply + data) (count data)))))

(assert (= 37 (apply + (map (partial distance (mean test-data)) test-data))))

(assert (= 168 (apply + (map (partial (comp triangle distance) (avg-int test-data))
                             test-data))))

(comment
  (def data (parse (slurp "input/y2021/day7-input.txt")))

  (= 328262 (calc-min-f distance data))
  (= 90040997 (calc-min-f (comp triangle distance) data))

  (time (= 328262 (apply + (map (partial distance (mean data)) data))))
  ;; Not working, need something besides "average index"
  (avg-int data)
  ;; => 465 (464.546)
  ;; Optimal index is 464
  (= 90040997 (apply + (map (partial (comp triangle distance) 464)
                            data)))
  ;;
  )