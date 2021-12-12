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

(defn is-small-cave? [x]
  (re-matches #"[a-z]+" x))

(defn is-large-cave? [x]
  (re-matches #"[A-Z]+" x))

(defn can-visit? [path x]
  (not (and (is-small-cave? x)
            ((set path) x))))

(defn can-visit-2? [path x]
  (or (not (is-small-cave? x))
      (or (->> (frequencies (filter is-small-cave? path))
               (every? (fn [[_ v]]
                         (= v 1))))
          (not ((set path) x)))))

(defn paths [f path-map]
  (loop [paths (mapv (fn [x] ["start" x]) (path-map "start"))
         loops 0]
    (let [new-paths (into [] (mapcat (fn [p]
                                       (if (= "end" (last p))
                                         [p]
                                         (let [branches (remove #{"start"} (path-map (last p)))]
                                           (->> branches
                                                (filter (partial f p))
                                                (map (partial conj p)))))))
                          paths)]
      (when (> loops 100) (throw (ex-info "Loopy" {})))
      (if (= new-paths paths)
        paths
        (recur new-paths (inc loops))))))

(assert (= 10 (count (paths can-visit? (path-map (text->pairs test-data))))))
(assert (= 36 (count (paths can-visit-2? (path-map (text->pairs test-data))))))

(comment
  (time (count (paths can-visit? (path-map (text->pairs (slurp "input/y2021/day12-input.txt"))))))

  (time (count (paths can-visit-2? (path-map (text->pairs (slurp "input/y2021/day12-input.txt")))))))