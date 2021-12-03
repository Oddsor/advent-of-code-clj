(ns advent-of-code-clj.y2021.dag3
  (:require [clojure.string :as str]))

(def test-data "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn parse [text]
  (str/split-lines text))

(defn bit-str-to-dec [bit-str]
  (read-string (str "2r" bit-str)))

(defn calculate-power-consumption [data]
  (->> data
       (apply map list)
       (map frequencies)
       (map (partial sort-by val))
       (map (juxt (comp first second)
                  ffirst))
       ((juxt (comp bit-str-to-dec
                    (partial apply str)
                    (partial map first))
              (comp bit-str-to-dec
                    (partial apply str)
                    (partial map second))))
       (apply *)))

(assert (= 198 (calculate-power-consumption (parse test-data))))
(comment
  (calculate-power-consumption (parse (slurp "input/y2021/day3-input.txt"))))

(defn narrow-bits-by-fn [fun data]
  (reduce (fn [data idx]
            (if (= 1 (count data))
              (reduced data)
              (let [preferred-bit ({> \1
                                    < \0} fun)
                    sorted (->> data
                                (map #(nth % idx))
                                frequencies
                                (sort (fn [a b]
                                        (fun (val a) (val b)))))
                    to-take (if (every? #{(-> sorted first second)} (map second sorted))
                              preferred-bit
                              (ffirst sorted))]
                (filter (comp #{to-take} #(nth % idx)) data)))) 
          data (range (count (first data)))))

(assert (= 230 (let [data (parse test-data)]
                 (* (bit-str-to-dec (first (narrow-bits-by-fn > data)))
                    (bit-str-to-dec (first (narrow-bits-by-fn < data)))))))

(comment
  (let [data (parse (slurp "input/y2021/day3-input.txt"))]
    (* (bit-str-to-dec (first (narrow-bits-by-fn > data)))
       (bit-str-to-dec (first (narrow-bits-by-fn < data))))))