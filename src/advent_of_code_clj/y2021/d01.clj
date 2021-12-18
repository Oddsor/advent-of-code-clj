(ns advent-of-code-clj.y2021.d01
  (:require [clojure.string :as str]))

(def test-data "199
200
208
210
200
207
240
269
260
263")

(defn parse [text]
  (map #(Integer/parseInt %) (str/split-lines text)))

(defn part-1-recursive [text]
  (loop [[h & t] (parse text)
         last-measurement h
         increases 0]
    (if (nil? h)
      increases
      (recur t h (cond-> increases
                   (> h last-measurement) inc)))))

(defn measurements-higher-than-previous [measurements]
  (->> measurements
       (partition 2 1)
       (filter (partial apply <))
       count))

(assert (= 7 (part-1-recursive test-data)))
(assert (= 7 (measurements-higher-than-previous
              (parse test-data))))

(comment
  (measurements-higher-than-previous (parse (slurp "input/y2021/01.txt")))
  )

(defn sliding-sum [measurements]
  (->> measurements
       (partition 3 1)
       (map (partial apply +))))

(assert (= 5 (-> test-data
                 parse
                 sliding-sum
                 measurements-higher-than-previous)))

(comment
  (-> (slurp "input/y2021/01.txt")
      parse
      sliding-sum
      measurements-higher-than-previous)
  )

(comment
  ;; Minimal versjon
  ;; (let[i(read-string(str"["(slurp *in*)"]"))p #(partition %1 1 %2)m(fn[x](count(filter #(apply < %)(p 2 x))))s(fn[x](map #(apply + %)(p 3 x)))](prn[(m i)(-> i s m)]))
  ;; (let[i(read-string(str"["(slurp *in*)"]"))m(fn[n x](count(filter #(apply < %)(map list x(drop n x)))))](prn[(m 2 i)(m 3 i)]))
  (let [i (read-string (str "[" (slurp *in*) "]"))
        p #(partition %1 1 %2)
        m (fn [x] (count (filter #(apply < %) (p 2 x))))
        s (fn [x] (map #(apply + %) (p 3 x)))]
    (prn [(m i) (-> i s m)]))
  ;; Innsikt: deloppgave 2 tilsvarer å sammenligne verdi n og n+3, og droppe multiplisering
  (let [i (read-string (str "[" (slurp *in*) "]")) 
        m (fn [n x] (count (filter #(apply < %) (map list x (drop n x)))))] 
    (prn [(m 2 i) (m 3 i)])))