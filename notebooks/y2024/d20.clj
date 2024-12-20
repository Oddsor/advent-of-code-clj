(ns y2024.d20
  (:require
   [advent-of-code-clj.input :as input]
   [advent-of-code-clj.utils :as utils]
   [medley.core :as medley]))

; # 2024, dag 20

; ## Del 1

; Vi skal finne korteste sti i en labyrint, men denne gangen er
; twisten at man kan jukse én gang!

; Så i stedet for å kunne enkelt bruke en shortest path-algoritme
; må vi tenke litt annerledes.

; Hva med å "tabulere" hele kartet baklengs fra mål, gi hver node
; en "kost", og så finne alle mulige snarveier? Da kan man finne ut
; hvor mye tid som spares på å ta snarveien...

(def test-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

; Vi starter med å samle opp alle mulige posisjoner:

(defn coordinates [input]
  (let [coord-map (utils/coord-map-fixed (utils/text->matrix input))
        start-node (key (medley/find-first (fn [[_k v]]
                                             (= \S v))
                                           coord-map))
        end-node (key (medley/find-first (fn [[_k v]]
                                           (= \E v))
                                         coord-map))]
    {:start-node start-node
     :end-node end-node
     :possible-locations (keys (medley/filter-vals #{\. \S \E} coord-map))}))

(update (coordinates test-input) :possible-locations #(take 10 %))

; Så kan vi gå fra slutt-noden og finne ut hvor mange steg man må ta:

(defn node->cost [possible-locations from-node]
  (let [valid-location (set possible-locations)]
    (->> (utils/breadth-search
          (fn [node]
            (->> (apply utils/adjacent-hv node)
                 (filter valid-location)))
          from-node)
         (map-indexed (fn [idx items]
                        (into {} (map (fn [node] [node idx]))
                              items)))
         (apply merge))))

(let [{p :possible-locations
       from :end-node} (coordinates test-input)]
  (into {} (take 5) (node->cost p from)))

; Neste steg er å finne alle mulige snarveier. En snarvei er lik
; en mulig lokasjon som kan nå en annen mulig lokasjon selv om en
; umulig lokasjon er mellom.

(def dir-vectors
  (for [y [-1 0 1]
        x [-1 0 1]
        :when (not= x y)
        :when (or (zero? x) (zero? y))]
    [y x]))

dir-vectors

(defn shortcuts
  "For each possible location, can it reach a location two steps
   away with an impossible location in-between?"
  [possible-locations]
  (let [loc-set (set possible-locations)]
    (->> (for [loc-a possible-locations
               dir-v dir-vectors
               :let [next-v (mapv #(* % 2) dir-v)
                     pos-1 (mapv + loc-a dir-v)
                     pos-2 (mapv + loc-a next-v)]
               :when (and (not (loc-set pos-1))
                          (loc-set pos-2))]
           #{loc-a pos-2})
         set)))

; Nå ser vi at det er 44 mulige snarveier:

(count (shortcuts (:possible-locations (coordinates test-input))))

; Nå kan vi gå igjennom hver mulige snarvei, og se hvor mye tid som
; kan spares ved å bruke snarveien. Tiden spart finner vi ved å plusse
; sammen de korteste lengdene til snarvei-nodene fra start og slutt:

(defn cost-with-shortcut [node->cost-1 node->cost-2 shortcut]
  (let [c1 (mapv node->cost-1 shortcut)
        c2 (mapv node->cost-2 shortcut)]
    (+ (apply min c1) (apply min c2) 2)))

; Vi forventer at vi sparer 64 steg på å hoppe mellom slutt-noden
; og noden to steg til høyre:

(let [{:keys [possible-locations start-node end-node]} (coordinates test-input)
      node->cost-from-start (node->cost possible-locations start-node)
      node->cost-from-end (node->cost possible-locations end-node)
      cost (node->cost-from-start end-node)]
  (- cost (cost-with-shortcut
           node->cost-from-start
           node->cost-from-end
           #{[7 7] [7 5]})))

; Dette ser jo ut til å fungere! Da kan vi prøve å løse del 1:

(defn part-1 [required-amount-saved input]
  (let [{:keys [possible-locations start-node end-node]} (coordinates input)
        node->cost-from-start (node->cost possible-locations start-node)
        node->cost-from-end (node->cost possible-locations end-node)
        cost (node->cost-from-start end-node)
        shortcuts (shortcuts possible-locations)]
    (->> shortcuts
         (map #(- cost (cost-with-shortcut node->cost-from-start
                                           node->cost-from-end
                                           %)))
         (filter (fn [savings] (>= savings required-amount-saved)))
         count)))

; For å teste kan vi se om vi finner de 5 snarveiene som er beskrevet i oppgaveteksten
; som har en besparelse over 20 steg i test-dataene (~3ms kjøretid):

(delay (part-1 20 test-input))

; Det ser ut til å fungere også, så da finner vi nok løsningen på del 1 (~270 ms kjøretid):

(delay (time (part-1 100 (input/get-input 2024 20))))
