(ns advent-of-code-clj.y2021.dag6)

(def test-data (map read-string (re-seq #"\d+" "3,4,3,1,2")))

(defn step
  "Slow; updates list of fish by one day"
  [xs]
  (into (replace {-1 6} (map dec xs))
        (repeat (count (filter zero? xs)) 8)))

(defn step-mutative
  "Faster; but bounded by collection size in step 2"
  [days xs]
  (let [x (transient (into [] xs))]
    (dotimes [n days]
      (dotimes [i (count x)]
        (let [v (get x i)]
          (when (zero? v) (conj! x 8))
          (assoc! x i (if (zero? v)
                        6
                        (dec (get x i)))))))
    (persistent! x)))

(defn as-fishmap
  "Fish-map where key is days and value number of fish
   that have that day as its internal timer"
  [data]
  (frequencies data))

(defn update-fishmap
  "By using a fishmap we limit the size of the collection"
  [xs]
  (let [zeros (xs 0 0)
        +n (fnil + 0)]
    (-> (into {} (comp (map (fn [[k v]]
                              [(dec k) v]))
                       (remove (comp neg? first)))
              xs)
        (update 6 +n zeros)
        (update 8 +n zeros))))

(assert (= 5934 (count (last (take 81 (iterate step test-data))))))
(assert (= 5934 (count (step-mutative 80 test-data))))
(assert (= 5934 (apply + (vals (last (take 81 (iterate update-fishmap (as-fishmap test-data))))))))

(assert (= 26984457539 (apply + (vals (last (take 257 (iterate update-fishmap (as-fishmap test-data))))))))

(comment
  (def data (map read-string (re-seq #"\d+" (slurp "input/y2021/day6-input.txt"))))
  (= 377263 (count (last (take 81 (iterate step data)))))
  (= 1695929023803 (apply + (vals (last (take 257 (iterate update-fishmap (as-fishmap data)))))))
  ;
  )

(comment
  ;; "Oneliner", eller "onethreader"
  (->> (slurp "input/y2021/day6-input.txt")
       (re-seq #"\d+")
       (map read-string)
       frequencies
       (iterate (fn [xs]
                  (let [z (xs 0 0)]
                    (->> xs
                         (keep (fn [[k v]]
                                 (when-not (zero? k)
                                   {(dec k) v})))
                         (apply merge-with + {6 z 8 z})))))
       ((juxt #(nth % 80) #(nth % 256)))
       (map #(apply + (vals %))))
  ;
  )