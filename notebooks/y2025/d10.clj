(ns y2025.d10 
  (:require
    [clojure.edn :as edn]
    [advent-of-code-clj.utils :as utils]
    [advent-of-code-clj.input :as input]))

(def test-input
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn parse-line [line]
  (let [[target-str & remainder] (String/.split line " ")
        wiring (butlast remainder)
        joltage-requirements (last remainder)
        target (mapv (fn [x] (case x
                               "#" true
                               "." false))
                     (re-seq #"[#\.]" target-str))]
    {:target target
     :initial (vec (repeat (count target) false))
     :wiring (map (comp vec edn/read-string) wiring)
     :joltage-requirement joltage-requirements}))

(defn parse-input [input]
  (map parse-line (String/.split input "\n"))) 

(parse-input test-input)

(defn shortest-presses [input]
  (let [parsed (parse-input input)]
    (map (fn [{:keys [target initial wiring]}]
           (->> (utils/breadth-search (fn [current]
                                        (mapv (fn [indexes]
                                                (reduce (fn [acc idx]
                                                          (update acc idx not)) current indexes))
                                              wiring))
                                      initial)
                (take-while #(not-any? #{target} %))
                count))
         parsed)))

(reduce + (shortest-presses test-input))

(reduce + (shortest-presses (input/get-input 2025 10)))