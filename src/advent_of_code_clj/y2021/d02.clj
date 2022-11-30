(ns advent-of-code-clj.y2021.d02
  (:require [clojure.string :as str]))

(def test-data "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse
  [text]
  (map (fn [line]
         (let [[op amount] (str/split line #"\s")]
           {:op (keyword op)
            :amount (parse-long amount)}))
       (str/split-lines text)))

(defn update-position
  [{:keys [depth h-pos] :as state} {:keys [op amount]}]
  (merge state (condp = op
                 :down {:depth (+ depth amount)}
                 :up {:depth (- depth amount)}
                 :forward {:h-pos (+ h-pos amount)})))

(defn navigate-1 [ops]
  (reduce update-position {:depth 0 :h-pos 0} ops))

(defn update-position-with-aim
  [{:keys [aim depth h-pos] :as state} {:keys [op amount]}]
  (merge state (condp = op
                 :down {:aim (+ aim amount)}
                 :up {:aim (- aim amount)}
                 :forward {:h-pos (+ h-pos amount)
                           :depth (+ depth (* amount aim))})))

(defn navigate-2 [ops]
  (reduce update-position-with-aim {:depth 0 :h-pos 0 :aim 0} ops))

(defn multiply-positions [{:keys [depth h-pos] :as _state}]
  (* depth h-pos))

(comment
  ;; Part 1
  (multiply-positions (navigate-1 (parse (slurp "input/y2021/02.txt")))))

(comment
  ;; Part 2
  (multiply-positions (navigate-2 (parse (slurp "input/y2021/02.txt")))))

;; Asserts
(assert (= {:depth 10 :h-pos 15}
           (navigate-1 (parse test-data))))
(assert (= 150 (multiply-positions
                (navigate-1 (parse test-data)))))
(assert (= {:depth 60, :h-pos 15, :aim 10}
           (navigate-2 (parse test-data))))
(assert (= 900 (multiply-positions
                (navigate-2 (parse test-data)))))