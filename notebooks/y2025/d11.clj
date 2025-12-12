(ns y2025.d11 
  (:require
   [advent-of-code-clj.utils :as utils]
   [advent-of-code-clj.input :as input]))

(def test-input "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defn input->edges [input]
  (let [lines (utils/->lines (String/.replace input ":" ""))]
    (->> lines
         (map (fn [line]
                (let [[src & targets] (String/.split line " ")]
                  [(keyword src) (map keyword targets)])))
         (into {}))))

(defn find-all-paths [edges start end]
  (let [expand-trail (fn [trail]
                       (let [tip (peek trail)
                             next-tips (edges tip)]
                         (map (fn [t] (conj trail t)) next-tips)))]
    (loop [unfinished-trails #{[start]}
           finished-trails #{}]
      (let [next-trails (mapcat expand-trail unfinished-trails)
            {f true u false} (group-by (comp boolean #{end} peek)
                                       next-trails)
            n-finished (into finished-trails f)]
        (if (seq u)
          (recur u n-finished)
          n-finished)))))

(defn part-1 [input]
  (count (find-all-paths (input->edges input)
                         :you :out)))

(part-1 test-input)

(part-1 (input/get-input 2025 11))