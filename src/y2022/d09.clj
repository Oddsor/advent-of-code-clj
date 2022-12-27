(ns y2022.d09)

(defn parse [data]
  (->> data
       (re-seq #"\w+")
       (partition 2)
       (map (fn [[d n]]
              [(keyword d) (parse-long n)]))))

(defn movement-seq [moves]
  (mapcat (comp (partial apply repeat) reverse) moves))

(defn- neighbouring? [[hx hy] [tx ty]]
  (every? #(>= 1 (abs %)) [(- hx tx) (- hy ty)]))

(defn- move-relative-to [x y]
  ((condp apply [x y]
     = identity
     > inc
     < dec) y))

(defn move-towards-head [[hx hy] [tx ty]]
  (if (neighbouring? [hx hy] [tx ty])
    [tx ty]
    [(move-relative-to hx tx) (move-relative-to hy ty)]))

(defn move-tail [new-head [thead & tail]]
  (lazy-seq
   (cons new-head
         (when thead
           (move-tail (move-towards-head new-head thead) tail)))))

(defn move [[[hx hy] & tail] d]
  (move-tail [((case d :L dec :R inc identity) hx)
              ((case d :U inc :D dec identity) hy)]
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
  (= 6044 (part-1 (slurp "input/2022/09.txt")))
  (= 2384 (part-2 (slurp "input/2022/09.txt"))))