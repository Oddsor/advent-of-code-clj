(ns advent-of-code-clj.y2021.dag16
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def test-literal "D2FE28")
(def test-length-operation "38006F45291200")
(def test-count-operation "EE00D40C823060")

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
                                     [(first h) bin]))
                                 (str/split-lines xform-rules))))

(defn get-bits [t]
  (when t
    (lazy-cat (hex-to-binary (first t)) (get-bits (next t)))))

(defn packet-type [n]
  ({2r100 :literal
    0 :sum
    1 :product
    2 :min
    3 :max
    5 :gt
    6 :lt
    7 :eq} n))

(defn bin-xs-to-num [xs] (read-string (apply str "2r" xs)))

(defn get-literal [xs]
  (loop [xs xs
         numbits (list)]
    (let [c (first xs)
          g (concat numbits (take 4 (drop 1 xs)))]
      (if (= \1 c)
        (recur (drop 5 xs) g)
        [(bin-xs-to-num g) (drop 5 xs)]))))

(declare packets)

(defn get-operation [xs]
  (let [typeid (first xs)
        optype (if (= \1 typeid)
                 :pcount :clength)
        content-offset (drop (inc (if (= optype :pcount)
                                    11 15))
                             xs)]
    (if (= optype :pcount)
      ;; 11 bits representing number of packets
      (let [num-packets (bin-xs-to-num (take 11 (drop 1 xs)))]
        (loop [xs content-offset
               nums []
               n 0]
          (if (= n num-packets)
            [nums xs]
            (let [[p r] (packets xs)]
              (recur
               r
               (conj nums p)
               (inc n))))))
      ;; 15 bits representing length of subpackets
      (let [clength (bin-xs-to-num (take 15 (drop 1 xs)))]
        (loop [ps []
               xs (take clength content-offset)]
          (let [[p r] (packets xs)
                nps (conj ps p)
                nxs r]
            (if (seq nxs)
              (recur nps nxs)
              [nps (drop clength content-offset)])))))))

(defn packets [bits]
  (let [version (bin-xs-to-num (take 3 bits))
        ptype (packet-type (bin-xs-to-num (take 3 (drop 3 bits))))
        content-offset (drop 6 bits)
        [content remainder] (if (= :literal ptype)
                              (get-literal content-offset)
                              (get-operation content-offset))]
    [{:version version
      :type ptype
      :content content}
     (if (every? #{\0} remainder) nil remainder)]))

(packets (get-bits test-literal))
(packets (get-bits test-length-operation))
(packets (get-bits test-count-operation))

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

(defn operator [type]
  (case type
    :sum +
    :product *
    :min min
    :max max
    :gt (fn greater-than [a b]
          (if (> a b)
            1
            0))
    :lt (fn less-than [a b]
          (if (< a b)
            1
            0))
    :eq (fn equal [a b]
          (if (= a b)
            1 0))))

(defn calc [packet]
  (if (coll? (:content packet))
    (apply (operator (:type packet)) (map calc (:content packet)))
    (:content packet)))

(= 3 (calc (first (packets (get-bits "C200B40A82")))))
(= 54 (calc (first (packets (get-bits "04005AC33890")))))
(= 7 (calc (first (packets (get-bits "880086C3E88112")))))
(= 9 (calc (first (packets (get-bits "CE00C43D881120")))))
(= 1 (calc (first (packets (get-bits "D8005AC2A8F0")))))
(= 0 (calc (first (packets (get-bits "F600BC2D8F")))))
(= 0 (calc (first (packets (get-bits "9C005AC2F8F0")))))
(= 1 (calc (first (packets (get-bits "9C0141080250320F1802104A08")))))

(comment
  (= 943 (count-version-sum (first (packets (get-bits (slurp "input/y2021/16.txt"))))))
  (= 167737115857 
     (calc (first (packets (get-bits (slurp "input/y2021/16.txt"))))))
  ;
  )