(ns y2022.d16 
  (:require [clojure.string :as str]
            [ubergraph.alg :as ua]))

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

(defn determine-next-route [start-pos elapsed-time already-opened data]
  (letfn [(gain-at [x]
            (* (:pressure ((:end x) data)) (- (- 30 elapsed-time) (:cost x))))]
    (->> (keys data)
         (remove #(or (= start-pos %)
                      (already-opened %)
                      (zero? (:pressure (% data)))))
         (map (fn [end]
                (let [path (ua/shortest-path (fn [coord] (map (fn [x]
                                                                {:dest x})
                                                              (:next (coord data))))
                                             {:start-node start-pos
                                              :end-node end})]
                  (assoc path :gain (gain-at path)))))
         (sort #(> (:gain %1) (:gain %2)))
         (#(doto % clojure.pprint/pprint))
         first :list-of-edges deref
         (map second))))

(defn simulation [data {:keys [position move-to opened-valves elapsed-time pressure]}] 
  (tap> [(next move-to) (> (:pressure ((or (first move-to) position) data)) 0)])
  (let [npos (or (first move-to) position)
        next-moves (next move-to)
        opening-valve? (and (nil? next-moves)
                            (nil? (opened-valves npos))
                            (> (:pressure (npos data)) 0)) 
        nopened (if opening-valve? (conj opened-valves npos) opened-valves)] 
    {:position npos
     :move-to (or next-moves (when opening-valve? '()) (determine-next-route npos elapsed-time nopened data))
     :opened-valves nopened
     :elapsed-time (inc elapsed-time)
     :pressure (apply + pressure (map (comp :pressure data) opened-valves))}))

(defn part-1 [data] 
  (take 10 (iterate (partial simulation (parse data)) {:position :AA :move-to nil :opened-valves #{} :elapsed-time 0 :pressure 0})))

(= (list :CC :BB) (determine-next-route :DD 0 #{} (parse test-data)))

#_(assert (= 1651 (part-1 test-data)))