(ns advent-of-code-clj.y2022.d09)

(defn parse [data]
  (map (fn [[d n]] [(keyword d) (parse-long n)]) (partition 2 (re-seq #"\w+" data))))

(defn movement-seq [moves]
  (mapcat (comp (partial apply repeat) reverse) moves))

(defn- neighbouring? [[hx hy] [tx ty]]
  (and (>= 1 (abs (- hx tx))) (>= 1 (abs (- hy ty)))))

(defn move-towards-head [[hx hy] [tx ty]]
  (if (neighbouring? [hx hy] [tx ty])
    [tx ty]
    [(if (= hx tx) tx ((if (neg-int? (- hx tx)) dec inc) tx))
     (if (= hy ty) ty ((if (neg-int? (- hy ty)) dec inc) ty))]))

(defn move-tail [new-head [thead & tail]]
  (lazy-seq
   (cons new-head
         (when thead
           (move-tail (move-towards-head new-head thead) tail)))))

(defn move [[[hx hy] & tail] d]
  (move-tail (case d
               :R [(inc hx) hy]
               :L [(dec hx) hy]
               :U [hx (inc hy)]
               :D [hx (dec hy)])
             tail))

(defn build-chain [length]
  (repeat length [0 0]))

(defn find-unique-tail-positions [n moves]
  (->> (reductions move (build-chain n) (movement-seq moves))
       (map last)
       distinct
       count))

(defn part-1 [data]
  (find-unique-tail-positions 2 (parse data)))

(defn part-2 [data]
  (find-unique-tail-positions 10 (parse data)))

(comment
  (part-1 (slurp "input/2022/09.txt"))
  (part-2 (slurp "input/2022/09.txt")))