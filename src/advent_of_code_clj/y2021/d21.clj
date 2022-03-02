(ns advent-of-code-clj.y2021.d21
  (:require [clojure.string :as str]))

(defn parse [t]
  (let [[p1 p2] (str/split-lines t)
        p1-start (read-string (last (re-seq #"\d+" p1)))
        p2-start (read-string (last (re-seq #"\d+" p2)))]
    {:p1 p1-start :p2 p2-start}))

(def test-data (parse "Player 1 starting position: 4
Player 2 starting position: 8"))

(defn modpos [new-position]
  (inc (mod (dec new-position) 10)))

(defn play-game [data]
  (reduce (fn [{:keys [die] :as state} player-turn]
            (let [dice (range (inc die) (+ 4 die))
                  player-pos (modpos (+ (player-turn state)
                                        (apply + dice)))
                  new-state
                  (-> state
                      (assoc :die (+ 3 die))
                      (assoc player-turn player-pos)
                      (update-in [:scores player-turn] + player-pos))]
              (if (>= (get-in new-state [:scores player-turn]) 1000)
                (reduced new-state)
                new-state)))
          (assoc data
                 :die 0
                 :scores {:p1 0 :p2 0}) (cycle [:p1 :p2])))

(assert (= 739785
           (let [res (play-game test-data)]
             (* (apply min (vals (get res :scores)))
                (:die res)))))

(comment
  (assert (= 734820
             (let [res (play-game (parse (slurp "input/y2021/21.txt")))]
               (* (apply min (vals (get res :scores)))
                  (:die res))))))

;; ===
;; Too slow! I got stuck on this and couldn't give the problem more time
;; ===

(def flip-player {:p1 :p2 :p2 :p1})
(def to-num {\0 0 \1 1 \2 2})

(defn play-game-2 [data nth-die]
  (reduce (fn [{:keys [die current-player] :as state} nth-die]
            (let [player-pos (modpos (+ (get state current-player)
                                        (+ die (to-num nth-die))))
                  new-state
                  (-> state
                      (assoc :die (+ 3 die))
                      (update :current-player flip-player)
                      (assoc current-player player-pos)
                      (update-in [:scores current-player] + player-pos))]
              (if (>= (get-in new-state [:scores current-player]) 21)
                (reduced new-state)
                new-state)))
          (assoc data
                 :die 0
                 :current-player :p1
                 :scores {:p1 0 :p2 0})
          nth-die))

(comment
  (let [max-possibilities 20000000000000000
        wins {:p1 0 :p2 0}]
    (loop [n 10000000000000000
           wins wins]
      (when (= 0 (mod n 100000))
        (prn n))
      (let [uw (update wins (ffirst (sort-by val > (:scores (play-game-2 test-data (reverse (Long/toString n 3))))))
                       inc)]
        (if (> n max-possibilities)
          uw
          (recur (inc n) uw))))))
