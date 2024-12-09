(ns y2024.d09
  (:require
   [advent-of-code-clj.input :as input]
   [tech.v3.datatype-api :as dtype]
   [tech.v3.datatype.argops :as darg]
   [medley.core :as medley])
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
              (map-indexed (fn [idx x]
                             (if x (* idx x)
                                 0))))
             + drive))

(= 1928 (checksum (defragment (expand-drive test-input))))

(= 6211348208140 (checksum (defragment (expand-drive (input/get-input 2024 9)))))

; # Del 2

(defn lazy-defrag [input]
  (loop [drive (dtype/->array-buffer (expand-drive input))
         [id & ids] (sort > (disj (set drive) nil))]
    (if id
      (let [val-group (darg/argfilter #(= id %) drive)
            target-nil-group (->> (darg/argpartition-by some? drive)
                                  (keep #(when (false? (first %))
                                           (second %)))
                                  (filter #(>= (count %) (count val-group)))
                                  first)]
        (if (or (nil? target-nil-group)
                (> (first target-nil-group) (first val-group)))
          (recur drive ids)
          (recur
           (reduce (fn [d [idx vid]]
                     (-> d
                         (dtype/set-value! (target-nil-group idx) (d vid))
                         (dtype/set-value! vid nil))) drive (medley/indexed val-group))
           ids)))
      drive)))

(checksum (lazy-defrag test-input))

(comment
  (checksum (lazy-defrag  (input/get-input 2024 9))))
