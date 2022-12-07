(ns advent-of-code-clj.y2022.d07 
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn process-line [pwd ls-at tree line]
  (if (str/starts-with? line "$")
    (let [[command argument] (str/split (subs line 2) #"\s")]
      {:tree tree
       :pwd (if (= command "cd")
              (cond
                (= "/" argument) []
                (= ".." argument) (vec (butlast pwd))
                :else (conj pwd argument))
              pwd)
       :ls-at (if (= command "ls")
                argument
                nil)})
    (let [[size filename] (str/split line #"\s")]
      {:tree (if-let [size-n (parse-long size)]
               (assoc-in tree (filterv some? (concat pwd [ls-at filename])) size-n)
               tree)
       :pwd pwd
       :ls-at ls-at})))

(defn build-tree [data]
  (let [lines (str/split-lines data)]
    (->> lines
         (reduce (fn [{:keys [pwd ls-at tree]} line]
              (process-line pwd ls-at tree line))
            {:pwd [] :ls-at nil :tree {}})
         :tree)))

(defn metadata-to-tree [tree]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (let [sum-size (reduce + (concat
                                               (filter number? (vals x))
                                               (map :size (filter map? (vals x)))))]
                       {:size sum-size
                        :children x})
                     x)) 
                 tree))

(defn- directory-sizes [tree-with-sizes]
  (keep :size (tree-seq map? vals tree-with-sizes)))

(defn part-1 [data]
  (let [sizes (-> data build-tree metadata-to-tree directory-sizes)]
    (transduce (filter #(>= 100000  %)) + sizes)))

(defn part-2 [data]
  (let [tree-with-sizes (-> data build-tree metadata-to-tree)
        volume-to-delete (- 30000000 (- 70000000 (:size tree-with-sizes)))] 
    (apply min (filter #(<= volume-to-delete  %) (directory-sizes tree-with-sizes)))))

(comment 
  (part-1 (slurp "input/2022/07.txt")) 
  (part-2 (slurp "input/2022/07.txt")))