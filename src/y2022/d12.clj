(ns y2022.d12
  (:require [advent-of-code-clj.utils :refer [emap text->matrix]]
            [ubergraph.alg :as ua]))

(defn parse [data]
  (let [m (text->matrix data)
        s&e (into {} (for [y (range (count m))
                           x (range (count (first m)))
                           :let [position (get-in m [y x])]
                           :when (#{\S \E} position)]
                       [position [y x]]))]
    {:start (s&e \S)
     :end (s&e \E)
     :map (emap (comp int #({\S \a \E \z} % %)) m)}))

(defn valid-adjacents [[y x] data]
  (->> [[y (dec x)] [y (inc x)] [(dec y) x] [(inc y) x]]
       (filter (fn [coord]
                 (when-let [nearby-height (get-in data coord nil)]
                   (>= (inc (get-in data [y x])) nearby-height))))))

(defn shortest-path [from to m]
  (:cost (ua/shortest-path (fn [node]
                             (map (fn [coord] {:dest coord}) (valid-adjacents node m)))
                           {:start-node from :end-node to})))

(defn part-1 [data]
  (let [{s :start e :end m :map} (parse data)]
    (shortest-path s e m)))

(defn part-2 [data]
  (let [{e :end m :map} (parse data)]
    (->> (for [y (range (count m))
               x (range (count (first m)))
               :when (= (int \a) (get-in m [y x]))]
           [y x])
         (keep #(shortest-path % e m))
         (apply min))))

(comment
  (= 380 (part-1 (slurp "input/2022/12.txt")))
  (= 375 (part-2 (slurp "input/2022/12.txt"))))