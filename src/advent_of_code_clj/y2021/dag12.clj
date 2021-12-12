(ns advent-of-code-clj.y2021.dag12)

(defn text->pairs [t]
  (partition 2 (re-seq #"\w+" t)))

(def test-data "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn path-map [xs]
  (->> xs
       (mapcat (fn [[a b]]
                 [[a b] [b a]]))
       (group-by first)
       (into {} (map (fn [[k v]]
                       [k (map second v)])))))

(defn is-large-cave? [x]
  (re-matches #"[A-Z]+" x))

(defn can-visit? [path x]
  (and (not= "start" x)
       (not (and (not (is-large-cave? x))
                 ((set path) x)))))

(defn paths [path-map]
  (loop [paths (mapv (fn [x] ["start" x]) (path-map "start"))
         loops 0]
    (let [new-paths (into [] (mapcat (fn [p]
                                       (if (= "end" (last p))
                                         [p]
                                         (let [branches (path-map (last p))]
                                           (->> branches
                                                (filter (partial can-visit? p))
                                                (map (partial conj p)))))))
                          paths)]
      (when (> loops 100) (throw (ex-info "Loopy" {})))
      (if (= new-paths paths)
        paths
        (recur new-paths (inc loops))))))

(= 10 (count (paths (path-map (text->pairs test-data)))))
(= 103 (count (paths (path-map (text->pairs test-data)))))

(comment
  (time (count (paths (path-map (text->pairs (slurp "input/y2021/day12-input.txt"))))))
  )