(ns advent-of-code-clj.y2021.dag16
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def xform-rules "0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111")

(def hex-to-binary (into {} (map (fn [line]
                                   (let [[h bin] (str/split line #" = ")]
                                     [(first h) bin])))
                         (str/split-lines xform-rules)))

(defn get-bits [t]
  (when t
    (lazy-cat (hex-to-binary (first t)) (get-bits (next t)))))

(defn packet-type [n]
  ({0 +
    1 *
    2 min
    3 max
    4 identity
    5 >
    6 <
    7 =} n))

(defn bin-xs-to-num [xs] (read-string (apply str "2r" xs)))

(defn get-literal [xs]
  (loop [xs xs
         numbits (list)]
    (let [c (first xs)
          g (concat numbits (take 4 (drop 1 xs)))]
      (if (= \1 c)
        (recur (drop 5 xs) g)
        [[(bin-xs-to-num g)] (drop 5 xs)]))))

(declare packets)

(defn get-operation [xs]
  (let [package-count? (= \1 (first xs))
        [cxs content-xs] (split-at (if package-count?
                                     11 15)
                                   (drop 1 xs))
        counter (bin-xs-to-num cxs)]
    (loop [ps []
           xs (cond->> content-xs
                (not package-count?) (take counter))]
      (if (or (empty? xs)
              (and package-count?
                   (= counter (count ps))))
        [ps (if package-count?
              xs (drop counter content-xs))]
        (let [[p remainder] (packets xs)]
          (recur (conj ps p) remainder))))))

(defn packets [bits]
  (let [version (bin-xs-to-num (take 3 bits))
        ptype (packet-type (bin-xs-to-num (take 3 (drop 3 bits))))
        content-offset (drop 6 bits)
        [content remainder] (if (= identity ptype)
                              (get-literal content-offset)
                              (get-operation content-offset))]
    [{:version version
      :op ptype
      :content content}
     (if (every? #{\0} remainder) nil remainder)]))

(defn count-version-sum [packet]
  (let [sum-version (volatile! 0)]
    (walk/postwalk (fn [x]
                     (when (and (map-entry? x)
                                (= :version (first x)))
                       (vswap! sum-version + (last x)))
                     x)
                   packet)
    @sum-version))

(assert (= 16 (count-version-sum (first (packets (get-bits "8A004A801A8002F478"))))))
(assert (= 12 (count-version-sum (first (packets (get-bits "620080001611562C8802118E34"))))))
(assert (= 23 (count-version-sum (first (packets (get-bits "C0015000016115A2E0802F182340"))))))
(assert (= 31 (count-version-sum (first (packets (get-bits "A0016C880162017C3686B18A3D4780"))))))

(defn coerce-int [x]
  (cond
    (false? x) 0
    (true? x) 1
    :else x))

(defn calc [{:keys [op content] :as packet}]
  (apply (comp coerce-int op)
         (map (if (= identity op) identity calc)
              content)))

(assert (= 3 (calc (first (packets (get-bits "C200B40A82"))))))
(assert (= 54 (calc (first (packets (get-bits "04005AC33890"))))))
(assert (= 7 (calc (first (packets (get-bits "880086C3E88112"))))))
(assert (= 9 (calc (first (packets (get-bits "CE00C43D881120"))))))
(assert (= 1 (calc (first (packets (get-bits "D8005AC2A8F0"))))))
(assert (= 0 (calc (first (packets (get-bits "F600BC2D8F"))))))
(assert (= 0 (calc (first (packets (get-bits "9C005AC2F8F0"))))))
(assert (= 1 (calc (first (packets (get-bits "9C0141080250320F1802104A08"))))))

(comment
  ;; Part 1
  (= 943 (count-version-sum (first (packets (get-bits (slurp "input/y2021/16.txt"))))))
  ;; Part 2
  (= 167737115857
     (calc (first (packets (get-bits (slurp "input/y2021/16.txt"))))))
  ;
  )