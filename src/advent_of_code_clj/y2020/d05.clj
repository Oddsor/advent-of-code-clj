(ns advent-of-code-clj.y2020.d05
  (:require [com.rpl.specter :refer :all #_[srange-dynamic subselect ALL FIRST select-one]]))

(def test-data
  [["BFFFBBFRRR" 70 7 567]
   ["FFFBBBFRRR" 14 7 119]
   ["BBFFBBFRLL" 102 4 820]])

(def seats (map (fn [row]
                  (map (fn [col]
                         (+ (* row 8) col)) (range 8))) (range 128)))

(def to-sym (comp deref resolve symbol str))

(def F (srange-dynamic (constantly 0) #(/ (count %) 2)))
(def B (srange-dynamic #(/ (count %) 2) count))
(def L (subselect [ALL F] identity))
(def R (subselect [ALL B] identity))

(select-one (concat (map to-sym "BFFFBBFRRR")
                    [FIRST FIRST])
            seats)

(defn seat-info [boarding-code]
  (let [{:keys [row-range col-range]}
        (reduce (fn [{:keys [row-range col-range]} letter]
                  {:row-range (cond
                                (= \F letter) (take (/ (count row-range) 2) row-range)
                                (= \B letter) (drop (/ (count row-range) 2) row-range)
                                :else row-range)
                   :col-range (cond
                                (= \L letter) (take (/ (count col-range) 2) col-range)
                                (= \R letter) (drop (/ (count col-range) 2) col-range)
                                :else col-range)})
                {:row-range (range 128)
                 :col-range (range 8)}
                boarding-code)
        row (first row-range)
        col (first col-range)]
    {:row row
     :col col
     :seat-id (+ (* row 8) col)}))

(comment
  ;Høyeste registrerte boarding pass
  (->> (slurp "input/y2020/05.txt")
       (str/split-lines)
       (map (comp :seat-id seat-info))
       (apply max)))

(comment
  ; Ledige seter som eksisterer
  (let [registrerte-sete-ider (->> (slurp "input/y2020/05.txt")
                                   (str/split-lines)
                                   (map (comp :seat-id seat-info))
                                   set)
        alle-sete-ider (set (range (+ (* 127 8) 7)))]
    (->> alle-sete-ider
         (remove registrerte-sete-ider)
         (filter (fn [sete-id]
                   (and (registrerte-sete-ider (inc sete-id))
                        (registrerte-sete-ider (dec sete-id))))))))
