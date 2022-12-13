(ns y2022.d11
  (:require [clojure.string :as str]))

(defn find-long [str]
  (some-> (re-find #"\d+" str) parse-long))
(defn find-longs [str]
  (map parse-long (re-seq #"\d+" str)))

(defn op-or-num [x]
  (if (re-matches #"\d+" x) (parse-long x) (symbol x)))

(defn create-op [line]
  (let [[arg1 op arg2] (str/split (last (str/split line #" = ")) #" ")
        ops (symbol op) arg1 (op-or-num arg1) arg2 (op-or-num arg2)]
    (eval (list 'fn ['old] (list ops arg1 arg2)))))

(defn parse [data]
  (let [ms (str/split data #"\n\n")]
    (->> ms
         (map (fn [m]
                (let [monkey-lines (str/split-lines m)
                      mn (find-long (first monkey-lines))
                      items (find-longs (nth monkey-lines 1))
                      mo (create-op (nth monkey-lines 2))
                      mt (find-long (nth monkey-lines 3))
                      mtrue (find-long (nth monkey-lines 4))
                      mfalse (find-long (nth monkey-lines 5))]
                  [mn {:items items
                       :inspected 0
                       :op mo :div-by mt
                       :if-true mtrue :if-false mfalse}])))
         (into {}))))

(defn monkey-around [worry-reduction]
  (fn [monkeys]
    (reduce (fn [acc mnr]
              (let [monkey (get acc mnr)
                    new-items (map #(worry-reduction ((:op monkey) %)) (:items monkey))
                    {fitems false
                     titems true} (group-by #(zero? (mod % (:div-by monkey))) new-items)]
                (-> acc
                    (update-in [mnr :inspected] + (count new-items))
                    (update-in [(:if-true monkey) :items] concat titems)
                    (update-in [(:if-false monkey) :items] concat fitems)
                    (assoc-in [mnr :items] [])))) monkeys (keys monkeys))))

(defn inspected-multiplication [monkeys]
  (apply * (take-last 2 (sort (map (fn [[_ v]] (:inspected v)) monkeys)))))

(defn part-1 [data]
  (inspected-multiplication (nth (iterate (monkey-around (fn [x] (quot x 3))) (parse data)) 20)))

(defn part-2 [data]
  (let [monkeys (parse data)
        common-modulo (apply * (map (comp :div-by val) monkeys))]
    (inspected-multiplication (nth (iterate (monkey-around (fn [x] (mod x common-modulo))) monkeys) 10000))))

(comment (part-1 (slurp "input/2022/11.txt"))
         (part-2 (slurp "input/2022/11.txt")))