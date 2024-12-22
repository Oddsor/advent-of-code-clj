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

(defn distance [[ay ax] [by bx]]
  (+ (- (max ay by) (min ay by))
     (- (max ax bx) (min ax bx))))

(distance [7 5] [7 7])
(distance [3 1] [7 3])

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

(delay (part-1 100 (input/get-input 2024 20)))

; ## Del 1 med kun ett breadt-first-søk

; Tydeligvis er både test-input og reell input ikke bygd opp med blindgater og lignende,
; så man trenger ikke å kjøre bredde-først fra begge retninger:

(defn savings [node->cost shortcut]
  (let [node-costs (map node->cost shortcut)]
    (- (apply max node-costs) (apply min node-costs) 2)))

(let [{:keys [possible-locations start-node]} (coordinates test-input)
      node->cost-from-start (node->cost possible-locations start-node)]
  [(savings node->cost-from-start #{[7 7] [7 5]})
   (savings node->cost-from-start #{[3 1] [3 3]})
   (savings node->cost-from-start #{[1 7] [1 9]})])

(defn part-1-faster [required-amount-saved input]
  (let [{:keys [possible-locations start-node]} (coordinates input)
        node->cost-from-start (node->cost possible-locations start-node)
        shortcuts (shortcuts possible-locations)]
    (->> shortcuts
         (map #(savings node->cost-from-start %))
         (filter (fn [savings] (>= savings required-amount-saved)))
         count)))

(delay (part-1-faster 20 test-input))

(delay (part-1-faster 100 (input/get-input 2024 20)))

; ## Del 2

; I del 2 lærer vi at vi kan jukse enda lenger enn to steg! Da er det på tide å tenke
; nytt; hva er det vi trenger å vite?

; Vi må vite hvor mye tid vi kan spare ved å jukse mellom to noder, altså kan vi lete
; etter alle noder som er innen rekkevidde fra en gitt node, og hvor mye tid vi ville
; spart om vi jukset.

(defn shortcuts-new
  "For each possible location, can it reach a location two steps
   away with an impossible location in-between?"
  [cheat-length required-amount-saved node->cost]
  (let [nodes (mapv key (sort-by val node->cost))]
    (->> (for [index (range (count nodes))
               :let [loc-a (nth nodes index)]
               loc-b (drop (inc index) nodes)
               :when (not= loc-a loc-b) ; Not the same node
               :let [distance (distance loc-a loc-b)
                     time-saved (- (node->cost loc-b) (node->cost loc-a) distance)]
               :when (>= time-saved required-amount-saved) ; Would save required amount of time
               :when (>= cheat-length distance) ; Within distance of eachother?
               ]
           {#{loc-a loc-b} time-saved})
         (apply merge))))

; Den nye algoritmen fungerer fortsatt på del 1 (hvor cheat-length er 2):

(def test-node->cost (let [{:keys [possible-locations start-node]} (coordinates test-input)]
                       (node->cost possible-locations start-node)))

; Den eneste snarveien som sparer over 50 steg er mellom slutt-noden og noden
; til høyre på kartet:

(delay (shortcuts-new 2 50 test-node->cost))

; Og på del 2 (jukse rett fra start-noden til slutt-noden):

(delay ((shortcuts-new 20 50 test-node->cost) #{[3 1] [7 4]}))

(defn part-2 [required-amount-saved input]
  (let [{:keys [possible-locations start-node]} (coordinates input)
        node->cost-from-start (node->cost possible-locations start-node)]
    (count (shortcuts-new 20 required-amount-saved node->cost-from-start))))

; I test-inputten vil det være 285 snarveier som sparer inn mer enn 50 steg.

(delay (part-2 50 test-input))

; Dessverre tar algoritmen veldig lang tid på reell input (~11 sekunder), men
; vi får riktig svar (`989316`):
(comment
  (= 989316 (part-2 100 (input/get-input 2024 20))))
