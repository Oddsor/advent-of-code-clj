(ns y2022.d19
  (:require [advent-of-code-clj.utils :refer [sum]]
            [clojure.string :as str])
  (:import (java.util ArrayDeque)))

(def test-data "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn parse-blueprint [blueprint]
  (let [[blueprint-id
         ore-cost clay-cost
         obsidian-ore-cost obsidian-clay-cost
         geode-ore-cost geode-obsidian-cost] (map parse-long (re-seq #"\d+" blueprint))]
    {:id blueprint-id
     :ore-bot {:ore ore-cost}
     :clay-bot {:ore clay-cost}
     :obsidian-bot {:ore obsidian-ore-cost
                    :clay obsidian-clay-cost}
     :geode-bot {:ore geode-ore-cost
                 :obsidian geode-obsidian-cost}
     :max-ore-cost (max ore-cost obsidian-ore-cost geode-ore-cost)
     :geode-ore-cost (+ geode-ore-cost obsidian-ore-cost clay-cost)
     :max-clay-cost (max clay-cost obsidian-clay-cost)
     :max-obsidian-cost geode-obsidian-cost}))

(defn parse [data]
  (map parse-blueprint (str/split-lines data)))

(defn build-ore-bot [{{ore-cost :ore} :ore-bot :keys [max-ore-cost]}
                     [ore clay obsidian geode obot :as st]]
  (when (and (> max-ore-cost obot)
             (>= ore ore-cost))
    (-> st
        (update 0 - ore-cost)
        (update 4 inc))))

(defn build-clay-bot [{{ore-cost :ore} :clay-bot :keys [max-clay-cost]}
                      [ore clay obsidian geode obot cbot :as st]]
  (when (and (> max-clay-cost cbot)
             (>= ore ore-cost))
    (-> st
        (update 0 - ore-cost)
        (update 5 inc))))

(defn build-obsidian-bot [{{ore-cost :ore clay-cost :clay} :obsidian-bot :keys [max-obsidian-cost]}
                          [ore clay _ _ _ _ obsidian-bots :as st]]
  (when (and (> max-obsidian-cost obsidian-bots)
             (>= ore ore-cost)
             (>= clay clay-cost))
    (-> st
        (update 0 - ore-cost)
        (update 1 - clay-cost)
        (update 6 inc))))

(defn build-geode-bot [{{ore-cost :ore obsidian-cost :obsidian} :geode-bot}
                       [ore clay obsidian :as st]]
  (when (and (>= ore ore-cost)
             (>= obsidian obsidian-cost))
    (-> st
        (update 0 - ore-cost)
        (update 2 - obsidian-cost)
        (update 7 inc))))

(defn heuristic [blueprint [ore clay obsidian geode orebot claybot obsidianbot geodebot]]
  (float (+ geodebot
            (/ (+ geodebot
                  (/ (+ (/ orebot (-> blueprint :geode-bot :ore))
                        (/ obsidianbot (-> blueprint :geode-bot :obsidian)))
                     2)) 5)
            (/ claybot (-> blueprint :obsidian-bot :clay))
            (/ orebot (-> blueprint :geode-ore-cost))
            (/ obsidianbot (-> blueprint :max-obsidian-cost)))))

(defn mine [[ore clay obsidian geode obot cbot obbot gbot]]
  (vector-of :int (+ ore obot) (+ clay cbot) (+ obsidian obbot) (+ geode gbot) obot cbot obbot gbot))

(defn apply-blueprint [blueprint state c]
  (when-let [st (build-ore-bot blueprint state)] (conj! c st))
  (when-let [st (build-clay-bot blueprint state)] (conj! c st))
  (when-let [st (build-obsidian-bot blueprint state)] (conj! c st))
  (when-let [st (build-geode-bot blueprint state)] (conj! c st)))

(defn test-blueprint [to-n use-heuristic? blueprint]
  (let [fun (partial apply-blueprint blueprint)]
    (loop [n 0
           state [(vector-of :int 0 0 0 0 1 0 0 0)]]
      (let [most-geode-bots (apply max (map #(get % 7) state))]
        (if (= n to-n)
          (do (println "Blueprint complete!")
              (apply max (map #(get % 3) state)))
          (recur (inc n)
                 (->> state
                      (mapcat (fn [s]
                                (when-not (or #_(and (> n 7) (zero? (get s 5)))
                                           (and (> most-geode-bots 1) (zero? (get s 7)))
                                              (>= (- (inc most-geode-bots) (get s 7)) 2)
                                              #_(and (> n 20) (zero? (get s 7))))
                                  (let [[_ _ _ _ o a b g :as no-action] s
                                        c (transient [])]
                                    (fun s c)
                                    (cons (mine no-action)
                                          (map (fn [[x y z w ob cb obb gb]]
                                                 (vector-of :int
                                                            (+ x o)
                                                            (+ y a)
                                                            (+ z b)
                                                            (+ w g)
                                                            ob cb obb gb)) (persistent! c)))))))
                      distinct
                      ((fn [x]
                         (if use-heuristic?
                           (->> x
                                (sort-by (partial heuristic blueprint))
                                (take-last 15000))
                           x))))))))))

(defn part-1 [data]
  (sum (map-indexed (fn [idx bp]
                      (* (inc idx) (test-blueprint 24 false bp)))
                    (parse data))))

(defn part-2 [data]
  (apply * (map (fn [bp]
                  (test-blueprint 32 true bp))
                (take 3 (parse data)))))

(comment
  (assert (= 33 (part-1 test-data)))
  (assert (= 3472 (part-2 test-data)))
  (= 1766 (part-1 (slurp "input/2022/19.txt")))
  (= 30780 (part-2 (slurp "input/2022/19.txt"))))
