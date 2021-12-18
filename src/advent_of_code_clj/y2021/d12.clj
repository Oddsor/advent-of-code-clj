(ns advent-of-code-clj.y2021.d12)

(defn text->pairs [t]
  (partition 2 (re-seq #"\w+" t)))
(defn path-map [xs]
  (->> xs
       (mapcat (fn [[a b]]
                 [[a b] [b a]]))
       (group-by first)
       (into {} (map (fn [[k v]]
                       [k (map second v)])))))
(defn parse [t] (path-map (text->pairs t)))

(def test-data (parse "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))

(defn is-small-cave? [x]
  (and (not= "end" x)
       (re-matches #"[a-z]+" x)))

(defn small-cave-visited? [path x]
  (not (and (is-small-cave? x)
            (some #(= x %) path))))

(defn can-visit-2? [path x]
  (not (and (is-small-cave? x)
            (some #(= x %) path)
            (->> (frequencies (filter is-small-cave? path))
                 (some (fn [[_ v]]
                         (> v 1)))))))

(defn paths [f path-map]
  (loop [paths [["start"]]]
    (let [new-paths (mapcat (fn [[h :as p]]
                              (if (= "end" h)
                                (list p)
                                (sequence (comp (remove #{"start"})
                                                (filter (partial f p))
                                                (map #(cons % p)))
                                          (path-map h))))
                            paths)]
      (if (= new-paths paths) paths (recur new-paths)))))

(assert (= 10 (count (paths small-cave-visited? test-data))))
(assert (= 36 (count (paths can-visit-2? test-data))))

(comment
  (def data (parse (slurp "input/y2021/12.txt")))
  (time (= 3738 (count (paths small-cave-visited? data))))

  (time (= 120506 (count (paths can-visit-2? data)))))