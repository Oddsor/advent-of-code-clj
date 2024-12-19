^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2022.d02
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent-of-code-clj.input :as input]))

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

^{:nextjournal.clerk/visibility {:result :hide}}
(defn decide-outcome [{:keys [player opponent]}]
  (cond
    (= player opponent) :draw
    (= opponent (wins-to player)) :opponent-wins
    (= opponent (loses-to player)) :player-wins))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn calculate-score [{:keys [player] :as game}]
  (+ (outcome-points (decide-outcome game))
     (hand-points player)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (letfn [(calculate [line]
            (calculate-score
             {:opponent (opponent-strategy (first line))
              :player (player-strategy (last line))}))]
    (transduce (map (comp calculate)) +
               (str/split-lines data))))

(def desired-outcome {\X :lose
                      \Y :draw
                      \Z :win})
^{:nextjournal.clerk/visibility {:result :hide}}
(defn decide-hand [opponent desired-outcome]
  (case desired-outcome
    :draw opponent
    :win (wins-to opponent)
    :lose (loses-to opponent)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (letfn [(calculate [line]
            (let [opponent-hand (opponent-strategy (first line))]
              (calculate-score
               {:opponent opponent-hand
                :player (decide-hand opponent-hand (desired-outcome (last line)))})))]
    (transduce (map (comp calculate)) +
               (str/split-lines data))))

(= 11767 (part-1 (input/get-input 2022 2)))

(= 13886 (part-2 (input/get-input 2022 2)))
