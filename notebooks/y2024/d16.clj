^:kindly/hide-code
(ns y2024.d16
  (:require
    [advent-of-code-clj.utils :as utils]
    [ubergraph.alg :as ua]
    [medley.core :as medley]
    [advent-of-code-clj.input :as input]))

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

(defn neighbour-nodes-2 [{[y x] :dest}]
  [{:dest [(dec y) x]
    :direction :n}
   {:dest [(inc y) x]
    :direction :s}
   {:dest [y (inc x)]
    :direction :e}
   {:dest [y (dec x)]
    :direction :w}])

(defn node-filter-fn-2 [world]
  (fn [{:keys [dest]}]
    (= \. (world dest))))

(defn edge-cost-2 [[{dir-source :direction} {dir-target :direction}]]
  (if (= dir-source dir-target)
    1
    1001))

(defn djikstra [g filter-fn cost-fn start end]
  (loop [queue [start]
         visited #{}
         dist {(:dest start) 0}
         prev {}
         iterations 0]
    (if (> 1000 iterations)
      (let [edges (distinct (mapcat (fn [node]
                                      (for [neighbour (g node)
                                            :when (and (filter-fn neighbour)
                                                       (not (visited (:dest neighbour))))]
                                        [node neighbour]))
                                    queue))]
        (if (> (count edges) 0)
          (let [{n-dist :dist
                 n-prev :prev} (reduce (fn [{d :dist :as acc} [source neighbour :as e]]
                                         (let [old-dist-t (or (d (:dest neighbour)) Long/MAX_VALUE)
                                               old-dist-s (d (:dest source))
                                               edge-cost (cost-fn e)
                                               new-dist-n (+ old-dist-s edge-cost)]
                                           (if (>= old-dist-t new-dist-n)
                                             (->  acc
                                                  (assoc-in [:dist (:dest neighbour)] new-dist-n)
                                                  (update-in [:prev (:dest neighbour)]
                                                             (fn [x]
                                                               (let [x (or x #{})]
                                                                 (conj x (:dest source))))))
                                             acc)))
                                       {:dist dist :prev prev}
                                       edges)]
            (recur (map second edges)
                   (into visited (map :dest) queue)
                   n-dist
                   n-prev
                   (inc iterations)))
          [iterations dist prev]))
      nil)))

(let [{:keys [start end world]} (parse-data test-data)]
  (-> (djikstra neighbour-nodes-2
                (node-filter-fn-2 world)
                edge-cost-2
                {:dest start :direction :e}
                end)
      #_#_second (get [1 13])))

(defn part-2 [input]
  (let [{:keys [start end world]} (parse-data input)]
    (ua/shortest-path
      neighbour-nodes
      {:start-node [start :e] ; Start facing towards the east
       :end-node? (fn [[position _direction]]
                    (= end position))
       :node-filter (node-filter-fn world)
       :cost-fn edge-cost
       :traverse true})))

(let [paths (part-2 test-data)
      last-path (last paths)
      similar-cost (filter (fn [x]
                             (= (:cost x) (:cost last-path))) paths)]
  similar-cost)
