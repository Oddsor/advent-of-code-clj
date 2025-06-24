^{:nextjournal.clerk/visibility {:code :hide}
  :kindly/hide-code true}
(ns y2024.d12
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.utils :refer [find-neighbours] :as utils]
    [clojure.core.matrix :as mx]
    [nextjournal.clerk :as clerk]))

; # 2024, dag 12

; ## Del 1

; I dag skal vi finne regioner og kalkulere en "pris" for hver
; region som til slutt skal summeres.

; Eksempeldataene våre ser slik ut; regioner er **sammenhengende**
; sett med bokstaver:

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

; Som matrise:
(def test-matrix (utils/text->matrix test-input))

^{:nextjournal.clerk/viewer nextjournal.clerk/html
  :kind/hiccup true
  :kindly/hide-code true}
[:pre (with-out-str
        (mx/pm (utils/text->matrix test-input)))]

; Vi kommer til å bruke en map-representasjon av matrisen i oppgaven:

(def test-coordmap
  (utils/coord-map-fixed (utils/text->matrix test-input)))

; Første steg for å finne regionene er å kunne finne nabo-noder. Her har 
; vi en util-funksjon allerede. Funksjonen har et predikat-argument slik
; at vi kan filtrere ut nabo-noder som ikke er av samme type og som er
; innenfor matrisen:

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

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-region [coord-map node]
  (into [] cat
        (utils/breadth-search
          (fn [node]
            (find-neighbours #(= (coord-map node)
                                 (coord-map %))
                             node))
          node)))

; **OBS:** Noder med samme bokstav er ikke nødvendigvis i samme region.
; Se C'en som ligger for seg selv, den er en egen region:
^{:nextjournal.clerk/viewer nextjournal.clerk/html
  :kind/hiccup true
  :kindly/hide-code true}
[:pre
 (with-out-str
   (mx/pm test-matrix))]

; Å finne alle regioner gjøres ved å undersøke alle noder i matrisen,
; finne hele regionen noden tilhører. Vi unngår å finne samme region
; igjen ved å lagre alle oppdagede noder i et sett (visited)

^{:nextjournal.clerk/visibility {:result :hide}}
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

^{:nextjournal.clerk/visibility {:result :hide}}
(defn open-sides [node-set node]
  (->> node
       (apply utils/adjacent-hv)
       (remove node-set)
       count))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn circumference [nodes]
  (let [node-set (set nodes)]
    (transduce (map #(open-sides node-set %))
               + nodes)))

; "Prisen" i en region er areal multiplisert med omkrets:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn region-price [nodes]
  (let [area (count nodes)]
    (* area (circumference nodes))))

; Og løsningen på del 1 er summen av prisen i alle regioner:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        regions (find-regions coord-map)]
    (transduce (map region-price) + regions)))

(= 1930 (part-1 test-input))

(part-1 (input/get-input 2024 12))

; ## Del 2

; I del 2 skal vi finne antall sider i en region i stedet for omkretsen.

; Først må vi definere en algoritme for hva et "hjørne" er.
; Antar at man må se på et sett med 4 koordinater sammen for å avgjøre om
; vi har et hjørne.

(defn box-coordinates [x y]
  (for [x [x (inc x)]
        y [y (inc y)]]
    [x y]))

(box-coordinates 1 1)

(defn corner-boxes [[x y]]
  (let [top-left (box-coordinates (dec x) (dec y))
        top-right (box-coordinates x (dec y))
        bottom-left (box-coordinates (dec x) y)
        bottom-right (box-coordinates x y)]
    [top-left top-right bottom-left bottom-right]))

(corner-boxes [1 1])

; Gitt testmatrisen:

^{:nextjournal.clerk/viewer nextjournal.clerk/html
  :kind/hiccup true
  :kindly/hide-code true}
[:pre
 (with-out-str
   (mx/pm test-matrix))]

; Så skal regionen `R` øverst til høyre ha et hjørne på
; blant annet koordinat 0,0 og 1,2

(defn corners [node-set box]
  (let [node-or-nil (mapv node-set box)
        nodes-in-box (count (filter identity node-or-nil))]
    (case nodes-in-box
      (1 3) 1
      4 0
      ; If we're in a search box with diagonal nodes, count as two corners
      2 (let [[tl tr bl br] node-or-nil]
          (if (or (= nil tl br)
                  (= nil tr bl))
            2
            0)))))

(defn find-corners [nodes]
  (let [node-set (set nodes)]
    (->> (reduce (fn [{:keys [visited num-corners]} [y x]]
                   (let [boxes-to-test (remove visited (corner-boxes [y x]))
                         n-corners (->> boxes-to-test
                                        (map (fn [box]
                                               (corners node-set box)))
                                        (reduce + 0))]
                     {:visited (into visited boxes-to-test)
                      :num-corners (+ num-corners n-corners)}))
                 {:visited #{} :num-corners 0}
                 nodes)
         :num-corners)))

(find-corners (find-region test-coordmap [0 0]))

(defn region-discounted-price [nodes]
  (let [area (count nodes)]
    (* area (find-corners nodes))))

(defn part-2 [input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        regions (find-regions coord-map)]
    (transduce (map region-discounted-price) + regions)))

(part-2 test-input)

; En viktig "gotcha" er at vi må ta høyde for at to gjerder kan "møtes",
; så i eksempelet under har region `A` to hjørner i midten.

(def test-input-part-2 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")

(part-2 test-input-part-2)

(time
  (part-2 (input/get-input 2024 12)))

