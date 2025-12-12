(ns y2025.d11 
  (:require
   [advent-of-code-clj.utils :as utils]
   [advent-of-code-clj.input :as input]
   [medley.core :as medley]))

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
  (->> (String/.replace input ":" "")
       utils/->lines
       (map (fn [line]
              (let [[src & targets] (String/.split line " ")]
                [(keyword src) (map keyword targets)])))
       (into {})))

(defn valid-trail? [trail]
  (every? #(= 1 (val %)) (frequencies trail)))

(defn find-all-paths [edges start end]
  (let [expand-trail (fn [trail]
                       (let [tip (peek trail)
                             next-tips (edges tip)]
                         (map (fn [t] (conj trail t)) next-tips)))]
    (loop [unfinished-trails #{[start]}
           finished-trails #{}]
      (let [next-trails (->> unfinished-trails
                             (mapcat expand-trail)
                             (filter valid-trail?)) 
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

(def test-input-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defn remove-edges [edge-to-remove edges]
  (medley/map-vals (fn [outbound]
                     (vec (remove edge-to-remove outbound)))
                   edges))

(defn part-2 [input]
  (let [edges (input->edges input) 
        paths-containing-intermediate-stops
        (filter (fn [path]
                  (and (some #{:fft} path)
                       (some #{:dac} path)))
                (find-all-paths edges
                                :svr :out))]
    (count paths-containing-intermediate-stops)))

(time
  (part-2 test-input-2))

;; TODO too slow
#_(delay (time (part-2 (input/get-input 2025 11))))