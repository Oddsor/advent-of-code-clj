(ns y2022.d20
  (:require
    [clojure.edn :as edn]
    [com.rpl.specter :refer [before-index NONE nthpath setval]])
  (:import
    (java.util ArrayList List)))

(defn parse [data]
  (edn/read-string (str "[" data "]")))

(defn reorder
  ([xs] (reorder 1 xs))
  ([times xs]
   (let [size (count xs)
         with-ids (map-indexed (fn [idx n] {:id idx :number n}) xs)]
     (->> (if (> times 1)
            (take (* size times) (cycle with-ids))
            with-ids)
          (reduce
            (fn [acc {n :number :as entry}]
              (if (zero? n)
                acc
                (let [idx (List/.indexOf acc entry)
                      new-idx (mod (+ idx n) (dec size))]
                  (->> acc
                       (setval [(nthpath idx)] NONE)
                       (setval [(before-index new-idx)] entry)))))
            with-ids)
          (mapv :number)))))

(defn reorder-fast
  ([xs] (reorder-fast 1 xs))
  ([times xs]
   (let [size (count xs)
         with-ids (map-indexed (fn [idx n] {:id idx :number n}) xs)]
     (->> (if (> times 1)
            (take (* size times) (cycle with-ids))
            with-ids)
          (reduce
            (fn [acc {n :number :as entry}]
              (if (zero? n)
                acc
                (let [idx (ArrayList/.indexOf acc entry)
                      new-idx (mod (+ idx n) (dec size))]
                  (doto acc
                    (ArrayList/.remove ^int idx)
                    (ArrayList/.add new-idx entry)))))
            (ArrayList. ^List with-ids))
          (mapv :number)))))

(defn compute [xs]
  (let [zidx (List/.indexOf xs 0)]
    (transduce (comp (drop zidx)
                     (take-nth 1000)
                     (take 4)) + (cycle xs))))

(def ^:dynamic *reorder-function* reorder)

(defn part-1 [data] (compute (*reorder-function* (parse data))))
(defn part-2 [data] (->> data
                         parse
                         (mapv (partial * 811589153))
                         (*reorder-function* 10)
                         compute))

(comment
  (part-1 (slurp "input/2022/20.txt"))
  (part-2 (slurp "input/2022/20.txt"))
  (with-bindings {#'*reorder-function* reorder-fast} (part-1 (slurp "input/2022/20.txt")))
  (with-bindings {#'*reorder-function* reorder-fast} (part-2 (slurp "input/2022/20.txt"))))
