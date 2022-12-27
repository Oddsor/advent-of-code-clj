(ns y2022.d07
  (:require [advent-of-code-clj.utils :refer [fif sum]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn process-line [pwd tree line]
  (if-let [[command argument] (and (str/starts-with? line "$")
                                   (str/split (subs line 2) #"\s"))]
    {:tree tree
     :pwd (if (= command "cd")
            (cond
              (= "/" argument) []
              (= ".." argument) (vec (butlast pwd))
              :else (conj pwd argument))
            pwd)}
    (let [[size filename] (str/split line #"\s")]
      {:tree (if-let [size-n (parse-long size)]
               (assoc-in tree (filterv some? (conj pwd filename)) size-n)
               tree)
       :pwd pwd})))

(defn build-tree [data]
  (->> (str/split-lines data)
       (reduce (fn [{:keys [pwd tree]} line]
                 (process-line pwd tree line))
               {:pwd [] :tree {}})
       :tree))

(defn assign-size-to-directories [tree]
  (walk/postwalk (fif map? (fn [node]
                             {:size (sum (concat
                                          (filter number? (vals node))
                                          (keep :size (vals node))))
                              :children node}))
                 tree))

(defn- directory-sizes [tree-with-sizes]
  (keep :size (tree-seq map? vals tree-with-sizes)))

(defn part-1 [data]
  (let [sizes (-> data build-tree assign-size-to-directories directory-sizes)]
    (transduce (filter #(>= 100000  %)) + sizes)))

(defn part-2 [data]
  (let [tree-with-sizes (-> data build-tree assign-size-to-directories)
        volume-to-delete (- 30000000 (- 70000000 (:size tree-with-sizes)))]
    (apply min (filter #(<= volume-to-delete  %) (directory-sizes tree-with-sizes)))))

(comment
  (= 1778099 (part-1 (slurp "input/2022/07.txt")))
  (= 1623571 (part-2 (slurp "input/2022/07.txt"))))