(ns advent-of-code-clj.y2021.d18
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn branches [nest-level z]
  (if (= nest-level 0) [z]
      (let [children (take-while some? (iterate zip/right (zip/down z)))]
        (->> children
             (remove (comp number? zip/node))
             (mapcat (partial branches (dec nest-level)))))))

(defn go-until-number [fn z]
  (let [nz (fn z)]
    (cond
      (or (nil? nz) (zip/end? nz)) nil
      (number? (zip/node nz)) nz
      :else (recur fn nz))))

(defn explode [data]
  (if-let [xp (first (branches 4 (zip/vector-zip data)))]
    (let [[l r] (zip/node xp)
          nz (zip/replace xp 0)
          wal (if-let [pos (go-until-number zip/prev nz)]
                (go-until-number zip/next (zip/edit pos + l))
                nz)
          war (if-let [pos (go-until-number zip/next wal)]
                (zip/edit pos + r)
                wal)]
      (zip/root war))
    data))

(defn split-pos-if-num [z]
  (let [n (zip/node z)]
    (if (and (number? n)
             (>= n 10))
      (zip/replace z [(int (Math/floor (/ n 2)))
                      (int (Math/ceil (/ n 2)))])
      z)))

(defn split [data]
  (loop [z (zip/vector-zip data)]
    (let [nz (split-pos-if-num z)]
      (if (or (zip/end? z)
              (not= z nz))
        (zip/root nz)
        (recur (zip/next z))))))

(assert (= (explode [[[[[9,8],1],2],3],4])
           [[[[0,9],2],3],4]))

(assert (= (explode [7,[6,[5,[4,[3,2]]]]])
           [7,[6,[5,[7,0]]]]))

(assert (= (explode [[6,[5,[4,[3,2]]]],1])
           [[6,[5,[7,0]]],3]))

(assert (= (explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
           [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]))

(assert (= (explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
           [[3,[2,[8,0]]],[9,[5,[7,0]]]]))

(defn repeatedly-apply [f xs]
  (let [nxs (f xs)]
    (if (= nxs xs)
      nxs (recur f nxs))))

(assert (= (split [[[[0,7],4],[15,[0,13]]],[1,1]])
           [[[[0,7],4],[[7,8],[0,13]]],[1,1]]))

(assert (= (repeatedly-apply split [[[[0,7],4],[15,[0,13]]],[1,1]])
           [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]))

(defn reduce-snailnum [xs]
  (let [nxs (->> xs (repeatedly-apply explode) split)]
    (if (= xs nxs)
      nxs (recur nxs))))

(defn add [xs1 xs2]
  (reduce-snailnum [xs1 xs2]))

(assert (= (add [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]] [7,[[[3,7],[4,3]],[[6,3],[8,8]]]])
           [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]))

(defn magnitude [xs]
  (walk/postwalk (fn [x]
                   (if (and (coll? x)
                            (every? number? x))
                     (+ (* 3 (first x))
                        (* 2 (second x)))
                     x)) xs))

(assert (= 143 (magnitude [[1,2],[[3,4],5]])))
(assert (= 3488 (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))

(def test-part-2 [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                  [[[5,[2,8]],4],[5,[[9,9],0]]]
                  [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                  [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                  [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                  [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                  [[[[5,4],[7,7]],8],[[8,3],8]]
                  [[9,3],[[9,9],[6,[4,9]]]]
                  [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                  [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])

(defn max-magnitude [data]
  (apply max (reduce (fn [acc x]
                       (concat acc (mapcat (fn [y]
                                             (if (= x y) []
                                                 (map (comp magnitude (partial apply add))
                                                      [[x y] [y x]]))) data)))
                     [] data)))

(assert (= 3993 (max-magnitude test-part-2)))

(comment
  (def data (->> (slurp "input/y2021/18.txt")
                 str/split-lines
                 (map read-string)))
  (time (= 3756 (magnitude (reduce (completing add identity) data))))
  (time (= 4585 (max-magnitude data)))
  ;
  )