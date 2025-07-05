^:kindly/hide-code
(ns y2024.d16
  (:require
    [advent-of-code-clj.utils :as utils]
    [ubergraph.alg :as ua]
    [medley.core :as medley]
    [advent-of-code-clj.input :as input]
    [clojure.data.priority-map :as p]))

; # Dag 16: Reindeer Maze

; Her er oppgaven å finne korteste vei igjennom en labyrint. Nøtten som må
; knekkes her er at bevegelseskostnaden for å bevege seg ett steg frem er `1`,
; mens kostnaden for å snu seg 90 grader er `1000`!

; ## Del 1

; Gitt følgende test-data:

(def test-data "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

; Parse til en hashmap av koordinater, og hent start+slutt

(defn parse-data [input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        start (key (medley/find-first (comp #{\S} val) coord-map))
        end (key (medley/find-first (comp #{\E} val) coord-map))]
    {:world (assoc coord-map start \. end \.)
     :start start
     :end end}))

; Vi kan bruke ubergraph til å finne korteste vei igjennom labyrinten, men
; vi må ha noen støttefunksjoner.

; Grafen bygges opp ved å finne nabo-nodene til en posisjon, og retningen
; man må gå for å komme dit. Eventuelle endringer beregnes ved å sammenligne
; to noder i en kant (`edge-cost`)

(defn neighbour-nodes [[[y x] _direction]]
  [{:dest [[(dec y) x] :n]}
   {:dest [[(inc y) x] :s]}
   {:dest [[y (inc x)] :e]}
   {:dest [[y (dec x)] :w]}])

; Vi lager en funksjon for å filtrere nodene slik at vi ikke tar hensyn
; til noder som går inn i en vegg. Lager en høyere-ordens funksjon slik
; at vi har tilgang til data om labyrinten:

(defn node-filter-fn [world]
  (fn [[position _direction]]
    (= \. (world position))))

; Kostnaden av en bevegelse er `1`, med mindre retningen har endret seg,
; da legger vi til `1000` (altså blir kostnaden `1001`)

(defn edge-cost [[[_ direction-1] [_ direction-2]]]
  (if (= direction-1 direction-2)
    1
    1001))

(defn part-1 [input]
  (let [{:keys [start end world]} (parse-data input)]
    (ua/shortest-path
      neighbour-nodes
      {:start-node [start :e] ; Start facing towards the east
       :end-node? (fn [[position _direction]]
                    (= end position))
       :node-filter (node-filter-fn world)
       :cost-fn edge-cost})))

(part-1 test-data)

(part-1 (input/get-input 2024 16))

; ## Del 2

; I del 2 trenger vi å vite om alle nodene som ligger langs de korteste
; rutene igjennom labyrinten.

(defn opposite-direction [direction]
  (case direction
    :n :s :s :n :e :w :w :e))

(defn neighbour-nodes-2 [world]
  (fn [{[y x] :position dir :direction :as node}]
    (let [opposite (opposite-direction dir)]
      (filterv (fn [{:keys [position direction]}]
                 (and (#{\. \S \E} (world position))
                      (not= direction opposite)))
               [{:position [(dec y) x]
                 :direction :n}
                {:position [(inc y) x]
                 :direction :s}
                {:position [y (inc x)]
                 :direction :e}
                {:position [y (dec x)]
                 :direction :w}]))))

(defn edge-cost-2 [[{dir-source :direction} {dir-target :direction}]]
  (if (= dir-source dir-target)
    1
    1001))

(defn djikstra [g cost-fn end-node? start]
  (loop [queue (p/priority-map [start] 0)
         good-paths []
         seen {}
         min-cost Long/MAX_VALUE]
    (if (empty? queue)
      {:seen seen :min-cost min-cost :paths good-paths}
      (let [[path path-cost] (first queue)
            current (peek path)
            seen' (assoc seen current path-cost)
            queue' (pop queue)]
        (if (end-node? current)
          (cond (> min-cost path-cost)
                (recur queue' [path] seen' (long path-cost))
                (= min-cost path-cost)
                (recur queue' (conj good-paths path) seen' (long path-cost))
                :else
                (recur queue' good-paths seen' min-cost))
          (let [neighbour->cost (into {}
                                      (for [neighbour (g current)
                                            :let [cost (+ path-cost
                                                          (cost-fn [current neighbour]))]
                                            :when (> (or (seen neighbour) Long/MAX_VALUE)
                                                     cost)]
                                        [(conj path neighbour) cost]))]
            (recur (into queue' neighbour->cost)
                   good-paths
                   seen'
                   min-cost)))))))

(let [{:keys [start world end]} (parse-data (input/get-input 2024 16))
      end-node? (fn [node] (= end (:position node)))
      calculation (djikstra (neighbour-nodes-2 world)
                            edge-cost-2
                            end-node?
                            {:position start :direction :e})]
  (->> calculation :min-cost))

(let [{:keys [start world end]} (parse-data test-data)
      end-node? (fn [node] (= end (:position node)))
      calculation (djikstra (neighbour-nodes-2 world)
                            edge-cost-2
                            end-node?
                            {:position start :direction :e})]
  (->> calculation
       :min-cost))

(let [{:keys [start world end]} (parse-data test-data)
      end-node? (fn [node] (= end (:position node)))]
  (->> (djikstra (neighbour-nodes-2 world)
                 edge-cost-2
                 end-node?
                 {:position start :direction :e})
       :paths
       (mapcat #(map :position %))
       distinct count))

(let [{:keys [start world end]} (parse-data (input/get-input 2024 16))
      end-node? (fn [node] (= end (:position node)))]
  (->> (djikstra (neighbour-nodes-2 world)
                 edge-cost-2
                 end-node?
                 {:position start :direction :e})
       :paths
       (mapcat #(map :position %))
       distinct count))
