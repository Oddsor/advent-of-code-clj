(ns y2022.d02
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def opponent-strategy {\A :rock
                        \B :paper
                        \C :scissors})
(def player-strategy {\X :rock
                      \Y :paper
                      \Z :scissors})
(def outcome-points {:player-wins 6
                     :opponent-wins 0
                     :draw 3})
(def hand-points {:rock 1
                  :paper 2
                  :scissors 3})
(def loses-to {:paper :rock
               :scissors :paper
               :rock :scissors})
(def wins-to (set/map-invert loses-to))

(defn decide-outcome [{:keys [player opponent]}]
  (cond
    (= player opponent) :draw
    (= opponent (wins-to player)) :opponent-wins
    (= opponent (loses-to player)) :player-wins))

(defn calculate-score [{:keys [player] :as game}]
  (+ (outcome-points (decide-outcome game))
     (hand-points player)))

(defn part-1 [data]
  (letfn [(parse-line [line]
            {:opponent (opponent-strategy (first line))
             :player (player-strategy (last line))})]
    (apply + (map (comp calculate-score parse-line) (str/split-lines data)))))

(def desired-outcome {\X :lose
                      \Y :draw
                      \Z :win})
(defn decide-hand [opponent desired-outcome]
  (case desired-outcome
    :draw opponent
    :win (wins-to opponent)
    :lose (loses-to opponent)))

(defn part-2 [data]
  (letfn [(parse-line [line]
            (let [opponent-hand (opponent-strategy (first line))]
              {:opponent opponent-hand
               :player (decide-hand opponent-hand (desired-outcome (last line)))}))]
    (apply + (map (comp calculate-score parse-line) (str/split-lines data)))))

(comment
  (part-1 (slurp "input/2022/02.txt"))
  (part-2 (slurp "input/2022/02.txt")))
