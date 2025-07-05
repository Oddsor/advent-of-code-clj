^:kindly/hide-code
(ns y2024.d16
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.utils :as utils]
    [clojure.data.priority-map :as p]
    [medley.core :as medley]
    [ubergraph.alg :as ua]))

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

(defn opposite-direction [direction]
  (case direction
    :n :s :s :n :e :w :w :e))

(defn neighbour-nodes [world]
  (fn [[[y x] direction]]
    (let [opposite (opposite-direction direction)]
      (filter (fn [{[position direction] :dest}]
                (and (#{\. \S \E} (world position))
                     (not= direction opposite)))
              [{:dest [[(dec y) x] :n]}
               {:dest [[(inc y) x] :s]}
               {:dest [[y (inc x)] :e]}
               {:dest [[y (dec x)] :w]}]))))

; Kostnaden av en bevegelse er `1`, med mindre retningen har endret seg,
; da legger vi til `1000` (altså blir kostnaden `1001`)

(defn edge-cost [[[_ direction-1] [_ direction-2]]]
  (if (= direction-1 direction-2)
    1
    1001))

(defn part-1 [input]
  (let [{:keys [start end world]} (parse-data input)]
    (ua/shortest-path
      (neighbour-nodes world)
      {:start-node [start :e] ; Start facing towards the east
       :end-node? (fn [[position _direction]]
                    (= end position))
       :cost-fn edge-cost})))

(part-1 test-data)

(part-1 (input/get-input 2024 16))

; ## Del 2

; I del 2 trenger vi å vite om alle nodene som ligger langs de korteste
; rutene igjennom labyrinten. Satt lenge fast på denne før jeg fant løsningen
; til [Zelark](https://github.com/zelark/AoC/blob/master/src/zelark/aoc_2024/day_16.clj),
; som er en variant av Djikstra's algoritme hvor køen over noder som skal behandles
; er byttet ut fra `node->kostnad` til `path->kostnad`. På den måten kan man
; finne alle paths til mål med samme kostnad, og hente ut alle nodene som ligger
; langs en optimal path.

(defn djikstra-path [g cost-fn end-node? start]
  (loop [queue (p/priority-map [start] 0)
         good-paths []
         seen {}
         min-cost Long/MAX_VALUE]
    (if (empty? queue)
      {:seen seen :min-cost min-cost :paths good-paths}
      (let [[path path-cost] (peek queue)
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
          (let [neighbour->cost (into {} (for [neighbour (g current)
                                               :let [cost (+ path-cost
                                                             (cost-fn [current neighbour]))]
                                               :when (> (seen neighbour Long/MAX_VALUE)
                                                        cost)]
                                           [(conj path neighbour) cost]))]
            (recur (into queue' neighbour->cost)
                   good-paths
                   seen'
                   min-cost)))))))

; Med denne varianten av djikstra kan vi gjenbruke noen funksjoner fra
; del 1, og hente ut antall posisjoner langs en optimal vei igjennom
; labyrinten ved å telle opp distinkte koordinater.

(defn part-2 [input]
  (let [{:keys [start world end]} (parse-data input)
        end-node? (fn [[pos _dir :as _node]] (= end pos))
        {:keys [paths]} (djikstra-path (comp #(map :dest %)
                                             (neighbour-nodes world))
                                       edge-cost
                                       end-node?
                                       [start :e])]
    (->> paths
         (mapcat #(map first %))
         distinct
         count)))

; Og da har vi svaret for del 2

(part-2 test-data)

(delay (part-2 (input/get-input 2024 16)))
