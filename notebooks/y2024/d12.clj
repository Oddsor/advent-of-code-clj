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
   (mx/pm (mx/emap (fn [sym]
                     (if (= \C sym)
                       \C \.))
                   test-matrix)))]

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
       (find-neighbours (complement node-set))
       count))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn circumference [nodes]
  (let [node-set (set nodes)]
    (transduce (map #(open-sides node-set %))
               + nodes)))

; "Prisen" i en region er areal multiplisert med omkrets:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn circumference-price [nodes]
  (let [area (count nodes)]
    (* area (circumference nodes))))

; Og løsningen på del 1 er summen av prisen i alle regioner:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn total-price [price-fn input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        regions (find-regions coord-map)]
    (transduce (map price-fn) + regions)))

(= 1930 (total-price circumference-price test-input))

(total-price circumference-price (input/get-input 2024 12))

; ## Del 2

; I del 2 skal vi finne antall sider i en region i stedet for omkretsen.

; Først må vi definere en algoritme for hva et "hjørne" er.
; Antar at man må se på et sett med 4 koordinater sammen for å avgjøre om
; vi har et hjørne.

(defn square [x y]
  (for [x [x (inc x)]
        y [y (inc y)]]
    [x y]))

(square 1 1)

(defn corner-squares [[x y]]
  (let [top-left (square (dec x) (dec y))
        top-right (square x (dec y))
        bottom-left (square (dec x) y)
        bottom-right (square x y)]
    [top-left top-right bottom-left bottom-right]))

(corner-squares [1 1])

; I testdataene våre skal region `R` ha 10 sider:

^{:nextjournal.clerk/viewer nextjournal.clerk/html
  :kind/hiccup true
  :kindly/hide-code true}
[:pre
 (with-out-str
   (mx/pm (mx/emap (fn [sym]
                     (if (= \R sym)
                       \R \.))
                   test-matrix)))]

; Gitt et 2x2 søkeområde, så finner vi ett hjørne dersom
; det er 1 eller 3 noder fra regionen til stede, eller
; to hjørner dersom det er 2 som er plassert diagonalt:

(defn corners-in-square [node-set square]
  (let [node-or-nil (mapv node-set square)
        nodes-in-square (count (filter identity node-or-nil))]
    (case nodes-in-square
      (1 3) 1
      4 0
      ; If we're in a search square with diagonal nodes, count as two corners
      2 (let [[tl tr bl br] node-or-nil]
          (if (or (= nil tl br)
                  (= nil tr bl))
            2
            0)))))

(defn count-corners [nodes]
  (let [node-set (set nodes)]
    (->> (reduce (fn [{:keys [visited num-corners]} node]
                   (let [squares-to-test (remove visited (corner-squares node))
                         n-corners (->> squares-to-test
                                        (map (fn [square]
                                               (corners-in-square node-set square)))
                                        (reduce + 0))]
                     {:visited (into visited squares-to-test)
                      :num-corners (+ num-corners n-corners)}))
                 {:visited #{} :num-corners 0}
                 nodes)
         :num-corners)))

(count-corners (find-region test-coordmap [0 0]))

(defn sides-price [nodes]
  (let [area (count nodes)]
    (* area (count-corners nodes))))

(total-price sides-price test-input)

; En viktig "gotcha" er at vi må ta høyde for at to gjerder kan "møtes",
; så i eksempelet under har region `A` to hjørner i midten.

(def test-input-part-2 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")

(total-price sides-price test-input-part-2)

(total-price sides-price (input/get-input 2024 12))

