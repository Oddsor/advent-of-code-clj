(ns y2022.d12 
  (:require [clojure.string :as str] 
            [ubergraph.alg :as ua]))

(defn parse [data]
  (let [m (->> data str/split-lines (mapv (comp vec seq)))
        st (into {} (for [y (range (count m))
                          x (range (count (first m)))
                          :let [position (get-in m [y x])]
                          :when (#{\S \E} position)]
                      [position [y x]]))]
    {:start (st \S)
     :end (st \E)
     :map (mapv #(mapv int %) (-> m
                                  (assoc-in (st \S) \a)
                                  (assoc-in (st \E) \z)))}))

(defn adjacent [x y data] 
  (->> [[y (dec x)] [y (inc x)] [(dec y) x] [(inc y) x]]
       (keep (fn [xs]
               (when-let [v (get-in data xs nil)]
                 [xs v])))
       (into {})))

(defn valid-adjacents [[y x] data]
  (let [height (get-in data [y x])]
    (->> (adjacent x y data)
         (remove #(> (val %) (inc height))) 
         keys)))

(defn shortest-path [from to m]
  (:cost (ua/shortest-path (fn [node]
                             (map (fn [coord]
                                    {:dest coord}) (valid-adjacents node m)))
                           {:start-node from
                            :end-node to})))

(defn part-1 [data]
  (let [state (parse data)
        m (:map state)
        s (:start state)
        e (:end state)]
    (shortest-path s e m)))

(defn part-2 [data]
  (let [state (parse data)
        m (:map state)
        e (:end state)]
    (apply min (keep #(shortest-path % e m) (for [y (range (count m))
                                                  x (range (count (first m)))
                                                  :when (= (int \a) (get-in m [y x]))]
                                              [y x])))))

(comment (part-1 (slurp "input/2022/12.txt"))
         (part-2 (slurp "input/2022/12.txt")))