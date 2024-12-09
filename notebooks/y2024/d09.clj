(ns y2024.d09
  (:require
   [advent-of-code-clj.input :as input])
  (:import [java.util ArrayList]))

; # 2024, dag 9

; # Del 1

(def test-input "2333133121414131402")

(defn expand-drive [input]
  (let [numbers (partition-all 2 (map parse-long (re-seq #"\d" input)))
        id (volatile! -1)]
    (->> (for [[filled empty-space] numbers
               :let [id (vswap! id inc)]]
           (concat (repeat filled id)
                   (when empty-space
                     (repeat empty-space nil))))
         (mapcat identity)
         vec)))

(expand-drive test-input)

(defn defragment
  "Konverter til arraylist fordi vi kommer til å mutere ofte"
  [drive]
  (loop [drive (ArrayList. drive)]
    (let [first-nil-idx (.indexOf drive nil)
          last-num-idx (- (.size drive) 1)]
      (if (= -1 first-nil-idx)
        (vec drive)
        (recur (doto drive
                 (.set first-nil-idx (.get drive last-num-idx))
                 (.remove last-num-idx)))))))

(defragment (expand-drive test-input))

(defn checksum [drive]
  (transduce (comp
              (map-indexed (fn [idx v] (* idx v))))
             + drive))

(checksum (defragment (expand-drive test-input)))

(checksum (defragment (expand-drive (input/get-input 2024 9))))

; # Del 2

