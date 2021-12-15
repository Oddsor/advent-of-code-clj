(ns advent-of-code-clj.y2021.dag14
  (:require [clojure.string :as str]))

(defn parse [t]
  (let [lines (str/split-lines t)]
    {:freqs (assoc (frequencies (partition 2 1 (first lines)))
                   (list (last (first lines))) 1)
     :inserts (into {} (comp (drop 2)
                             (map #(str/split % #" -> "))
                             (map (fn [[p i]]
                                    (let [[a b] (seq p)
                                          c (first i)]
                                      [[a b] [[a c] [c b]]]))))
                    lines)}))

(def test-str "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def test-data (parse test-str))

(defn expand [rules freq-map]
  (->> freq-map
       (mapcat (fn [[k v]]
                 (map (fn [nk]
                        {nk v}) (rules k [k]))))
       (apply merge-with +)))

(defn max-minus-min [m]
  (->> m
       (map (fn [[k v]] {(first k) v}))
       (apply merge-with +)
       vals
       (apply (juxt max min))
       (apply -)))

(assert (= 1588
           (max-minus-min
            (nth (iterate (partial expand (:inserts test-data))
                          (:freqs test-data)) 10))))

(comment
  (def data (parse (slurp "input/y2021/14.txt")))

  (= 2590
     (max-minus-min
      (nth (iterate (partial expand (:inserts data))
                    (:freqs data)) 10)))

  (time 
   (= 2875665202438
      (max-minus-min
       (nth (iterate (partial expand (:inserts data))
                     (:freqs data)) 40))))
  ;
  )