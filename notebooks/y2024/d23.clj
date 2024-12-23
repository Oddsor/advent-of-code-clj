(ns y2024.d23
  (:require
   [advent-of-code-clj.input :as input]))

; # 2024, dag 23

; ## Del 1

(def test-input "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defn computers->connections [input]
  (->> (.split input "\n")
       (map (fn [line] (.split line "-")))
       (reduce (fn [acc [c1 c2]]
                 (-> acc
                     (update c1 (fnil conj #{}) c2)
                     (update c2 (fnil conj #{}) c1))) {})))

(computers->connections test-input)

#_(computers->connections (input/get-input 2024 23))

(defn find-interconnected-t-computers [computer->connection]
  (let [t-computers (filter #(.startsWith % "t") (keys computer->connection))]
    (reduce (fn [acc t-c]
              (let [connected (computer->connection t-c)]
                (into acc
                      (for [c connected
                            o connected
                            :when (and (not= c o)
                                       ((computer->connection c) o))]
                        #{t-c c o})))) #{} t-computers)))

(count (find-interconnected-t-computers (computers->connections test-input)))

(count (find-interconnected-t-computers (computers->connections (input/get-input 2024 23))))

; ## Del 2

(defn find-interconnected-computers [computer->connection]
  (for [[c1 connected] computer->connection]
    (conj (set (filter (fn [c2]
                         (and (some connected (computer->connection c2))
                              ((computer->connection c2) c1)))
                       connected))
          c1)))

(apply max-key val (frequencies (find-interconnected-computers (computers->connections test-input))))

(String/join "," ^Iterable (sort (key (apply max-key val (frequencies (find-interconnected-computers (computers->connections (input/get-input 2024 23))))))))
