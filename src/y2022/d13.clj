(ns y2022.d13 
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

(defn in-order? [lz rz]
  (let [ln (zip/node lz)
        rn (zip/node rz)] 
    (cond (zip/end? lz)
          true
          (zip/end? rz)
          false
          (and (number? ln) (number? rn))
          (if (= ln rn)
            (cond 
              (and (= lz (zip/rightmost lz)) 
                   (not= rz (zip/rightmost rz)))
              true
              (and (= rz (zip/rightmost rz))
                   (not= lz (zip/rightmost lz)))
              false
              :else (in-order? (zip/next lz) (zip/next rz)))
            (< ln rn))
          (and (number? rn) (coll? ln))
          (in-order? lz (zip/edit rz vector))
          (and (number? ln) (coll? rn))
          (in-order? (zip/edit lz vector) rz)
          :else (in-order? (zip/next lz) (zip/next rz)))))

(defn in-order-vec? [l r] 
  (cond (and (number? l) (number? r))
        (compare l r) 
        (and (number? r) (coll? l))
        (in-order-vec? l (vector r))
        (and (number? l) (coll? r))
        (in-order-vec? (vector l) r)
        :else (loop [[lh & lt] l
                     [rh & rt] r]
                (cond
                  (and (nil? lh) (nil? rh)) 0
                  (nil? lh) -1
                  (nil? rh) 1
                  :else 
                  (let [c (in-order-vec? lh rh)]
                    (if (zero? c)
                      (recur lt rt)
                      c))))))

(defn part-1 [data]
  (->> (str/split data #"\n\n")
       (map #(map read-string (str/split-lines %)))
       (map-indexed (fn [idx [l r]]
                      [(inc idx) (neg-int? (in-order-vec? l r))]))
       (reduce (fn [acc [idx in-order]]
                 (cond-> acc
                   in-order (+ idx))) 0)))

(defn part-2 [data]
  (let [dividers #{[[2]] [[6]]}
        packets (->> (str/split data #"\n\n")
                     (mapcat #(map read-string (str/split-lines %)))
                     (concat dividers))]
    (->> packets
         (sort in-order-vec?)
         (map-indexed (fn [idx packet]
                            [(inc idx) packet]))
         (keep (fn [[idx packet]]
                 (when (dividers packet)
                   idx)))
         (apply *))))


(comment 
  (part-1 (slurp "input/2022/13.txt"))
  (part-2 (slurp "input/2022/13.txt")))