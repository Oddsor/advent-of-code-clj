(ns advent-of-code-clj.dag6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-data-del-1 "abc

a
b
c

ab
ac

a
a
a
a

b")

(def test-data-del-2 "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn antall-besvarte-spørsmål-i-gruppe [gruppe]
  (count (set (mapcat identity gruppe))))

(defn antall-spørsmål-besvart-av-hele-gruppe [gruppe]
  (count (apply set/intersection (map set gruppe))))

(defn del-1 [data]
  (let [linjer (str/split-lines data)
        grupperte-svar (->> linjer
                            (partition-by #{""})
                            (remove #{'("")}))]
    (reduce + (map antall-besvarte-spørsmål-i-gruppe grupperte-svar))))

(defn del-2 [data]
  (let [linjer (str/split-lines data)
        grupperte-svar (->> linjer
                            (partition-by #{""})
                            (remove #{'("")}))]
    (reduce + (map antall-spørsmål-besvart-av-hele-gruppe grupperte-svar))))

(= 11 (del-1 test-data-del-1))

(= 6 (del-2 test-data-del-2))

(comment
  ; Del 1
  (del-1 (slurp "input/day6-input.txt"))

  ; Del 2
  (del-2 (slurp "input/day6-input.txt"))
  )