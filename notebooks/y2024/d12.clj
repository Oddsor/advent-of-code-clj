(ns y2024.d12
  (:require
   [advent-of-code-clj.input :as input]
   [advent-of-code-clj.utils :refer [find-neighbours] :as utils]
   [clojure.core.matrix :as m]
   [nextjournal.clerk :as clerk]))

; # 2024, dag 12

(def test-input "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(clerk/md
 (with-out-str
   (m/pm (utils/text->matrix test-input))))

(def test-coordmap
  (utils/coord-map-fixed (utils/text->matrix test-input)))

; ## Del 1

; Første steg er å kunne finne nabo-noder. Her har vi en util-funksjon
; allerede. Funksjonen har et predikat-argument slik at vi kan filtrere
; ut nabo-noder som ikke er av samme type og som er innenfor matrisen:

(find-neighbours identity [0 3])

(map test-coordmap (find-neighbours identity [0 3]))

(select-keys test-coordmap
             (find-neighbours test-coordmap [0 3]))

(select-keys test-coordmap
             (find-neighbours #(= (test-coordmap [0 3])
                                  (test-coordmap %))
                              [0 3]))

; Å finne en region er gjort ved å kjøre et bredde-først søk fra
; en gitt node, og lete etter alle noder som er av samme type (bokstav)

(defn find-region [coord-map node]
  (into [] cat
        (utils/breadth-search
         (fn [node]
           (find-neighbours #(= (coord-map node)
                                (coord-map %))
                            node))
         node)))

; Å finne alle regioner gjøres ved å undersøke alle noder i matrisen,
; finne hele regionen noden tilhører. Vi unngår å finne samme region
; igjen ved å lagre alle oppdagede noder i et sett (visited)

(defn find-regions [coord-map]
  (loop [visited #{}
         regions []
         [node & remaining] (map first coord-map)]
    (cond
      (nil? node) regions
      (visited node) (recur visited regions remaining)
      :else (let [region (find-region coord-map node)]
              (recur
               (into visited region)
               (conj regions region)
               remaining)))))

; Omkretsen til en region tilsvarer de "åpne sidene" til
; regionen sine noder. En region med én node har 4 åpne sider,
; som også er omkretsen. En firkantet region med 4 noder har
; omkrets lik 8, fordi hver node har to åpne sider.

(defn open-sides [node-set node]
  (->> node
       (apply utils/adjacent-hv)
       (remove node-set)
       count))

(defn circumference [nodes]
  (let [node-set (set nodes)]
    (transduce (map #(open-sides node-set %))
               + nodes)))

; "Prisen" i en region er areal multiplisert med omkrets:

(defn region-price [nodes]
  (let [area (count nodes)]
    (* area (circumference nodes))))

; Og løsningen på del 1 er summen av prisen i alle regioner:

(defn part-1 [input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        regions (find-regions coord-map)]
    (transduce (map region-price) + regions)))

(= 1930 (part-1 test-input))

(part-1 (input/get-input 2024 12))
