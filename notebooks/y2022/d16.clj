^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2022.d16
  (:require [nextjournal.clerk :as clerk]
            [taoensso.tufte :as t]))

;; # [Day 16: Proboscidea Volcanium](https://adventofcode.com/2022/day/16)

;; > The sensors have led you to the origin of the distress signal: yet another handheld device, just like the one the Elves gave you. However, you don't see any Elves around; instead, the device is surrounded by elephants! They must have gotten lost in these tunnels, and one of the elephants apparently figured out how to turn on the distress signal.

;; > The ground rumbles again, much stronger this time. What kind of cave is this, exactly? You scan the cave with your handheld device; it reports mostly igneous rock, some ash, pockets of pressurized gas, magma... this isn't just a cave, it's a volcano!

;; > You need to get the elephants out of here, quickly. Your device estimates that you have 30 minutes before the volcano erupts, so you don't have time to go back out the way you came in.

;; > You scan the cave for other options and discover a network of pipes and pressure-release valves. You aren't sure how such a system got into a volcano, but you don't have time to complain; your device produces a report (your puzzle input) of each valve's flow rate if it were opened (in pressure per minute) and the tunnels you could use to move between the valves.

;; > There's even a valve in the room you and the elephants are currently standing in labeled AA. You estimate it will take you one minute to open a single valve and one minute to follow any tunnel from one valve to another. What is the most pressure you could release?

;; > For example, suppose you had the following scan output:

^{::clerk/visibility {:result :hide}}
(def test-data "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

;; > All of the valves begin closed. You start at valve AA, but it must be damaged or jammed or something: its flow rate is 0, so there's no point in opening it. However, you could spend one minute moving to valve BB and another minute opening it; doing so would release pressure during the remaining 28 minutes at a flow rate of 13, a total eventual pressure release of 28 * 13 = 364. Then, you could spend your third minute moving to valve CC and your fourth minute opening it, providing an additional 26 minutes of eventual pressure release at a flow rate of 2, or 52 total pressure released by valve CC.

;; > Making your way through the tunnels like this, you could probably open many or all of the valves by the time 30 minutes have elapsed. However, you need to release as much pressure as possible, so you'll need to be methodical.

;; ## Part 1

;; Start by parsing the test-data into a map where each valve is a key, and the value is a map listing the pressure and neighbouring nodes:

^{::clerk/visibility {:result :hide}}
(require '[clojure.string :as str])

^{::clerk/visibility {:result :hide}}
(defn parse [data]
  (->> data
       str/split-lines
       (map (fn [line]
              (let [tokens (re-seq #"\w+" line)
                    [valve & connected-valves] (keep (fn [x]
                                                       (when (re-matches #"[A-Z]+" x)
                                                         (keyword x)))
                                                     tokens)
                    pressure (parse-long (some (partial re-matches #"\d+")
                                               tokens))]
                [valve {:next connected-valves
                        :pressure pressure}])))
       (into {})))

^{::clerk/visibility {:code :hide}}
(clerk/example
 (parse test-data))

;; To find the shortest path we'll use the excellent Ubergraph library:

^{::clerk/visibility {:result :hide}}
(require '[ubergraph.alg :as ua])
^{::clerk/visibility {:result :hide}}
(defn shortest-path [start-pos end-pos data]
  (ua/shortest-path (fn [coord]
                      (map (fn [neighbour] {:dest neighbour}) (-> data coord :next)))
                    {:start-node start-pos
                     :end-node end-pos}))

;; Once refactored and shrunk down, this function ended up being almost
;; a copy-paste of [@volesen's solution](https://github.com/volesen/aoc2022/blob/main/day16/day16.clj#L48C10-L48C10)

^{::clerk/visibility {:result :hide}}
(defn max-pressure [time-remaining start non-zero-valves elephant? data]
  (let [next-nodes* (keep (fn [node]
                            (let [time-to-open (inc (:cost (shortest-path start node data)))]
                              (when (pos? (- time-remaining time-to-open))
                                (max-pressure
                                 (- time-remaining time-to-open)
                                 node
                                 (disj non-zero-valves node)
                                 elephant?
                                 data))))
                          non-zero-valves)
        next-nodes (cond-> next-nodes*
                     elephant? (conj (max-pressure 26 :AA non-zero-valves false data)))]
    (+ (* time-remaining (-> data start :pressure)) (apply max 0 next-nodes))))

^{::clerk/visibility {:result :hide}}
(defn part-1 [data]
  (with-redefs [shortest-path (memoize shortest-path)]
    (let [data (parse data)
          non-zero-valves (set (remove (fn [valve] (zero? (:pressure (valve data)))) (keys data)))]
      (max-pressure 30 :AA non-zero-valves false data))))

^{::clerk/visibility {:code :hide}}
(clerk/example (= 1651 (part-1 test-data)))

;; Applying it to our input we get:
^{::clerk/visibility {:code :hide}}
(clerk/code
 '(= 1896 (part-1 (slurp "input/2022/16.txt"))))

^{::clerk/visibility {:result :hide}}
(defn part-2 [data-string]
  (with-redefs [shortest-path (memoize shortest-path)]
    (let [data (parse data-string)
          non-zero-valves (set (remove (fn [valve] (zero? (:pressure (valve data)))) (keys data)))]
      (max-pressure 26 :AA non-zero-valves true data))))
^{::clerk/visibility {:code :hide}}
(clerk/example (= 1707 (part-2 test-data)))

;; Applying it to our input we get:
^{::clerk/visibility {:code :hide}}
(clerk/code
 '(= 2576 (part-2 (slurp "input/2022/16.txt"))))