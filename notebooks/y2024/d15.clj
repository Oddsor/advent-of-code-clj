^:kindly/hide-code
(ns y2024.d15
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.utils :as utils]
    [clojure.core.matrix :as mx]
    [clojure.string :as str]
    [medley.core :as medley]))

; # Dag 15: Warehouse Woes

; Denne gangen skal vi simulere at en robot dytter bokser rundt i et
; varehus; løsningen er kalkulert ut fra sluttposisjonen til boksene.

; Jeg satt opprinnelig fast på del 1, med en litt dårlig fremgangsmåte.
; Etter å ha sett på en del andre løsninger ble jeg spesielt inspirert av
; løsningen til [Zelark](https://github.com/zelark/AoC/blob/master/src/zelark/aoc_2024/day_15.clj),
; som hadde en elegant algoritme som involverte bredde-først-søk for å samle
; opp alle boksene som står inntil hverandre og som skal bli forsøkt flyttet.

; ## Del 1

; Gitt følgende testdata:

(def test-data-small "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

; Eller følgende større eksempel

(def test-data-large "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

; Så kan vi hente ut en hashmap av koordinater, og posisjonen til roboten:

(defn map-and-moves [input]
  (let [[game-map-str moves-str] (String/.split input "\n\n")
        game-matrix (utils/text->matrix game-map-str)
        coord-map (utils/coord-map-fixed game-matrix)
        grouped (group-by val coord-map)]
    {:game-state {:coord-map (medley/remove-vals #{\. \@} coord-map)
                  :robot (->> (grouped \@) first key)}
     :moves (filter #{\^ \> \< \v} moves-str)}))

(map-and-moves test-data-small)

; Vi må kunne flytte både roboter og bokser i en retning:

(defn move-to-new-position [[old-y old-x] move]
  (case move
    \^ [(dec old-y) old-x]
    \> [old-y (inc old-x)]
    \v [(inc old-y) old-x]
    \< [old-y (dec old-x)]))

(defn print-board [{:keys [coord-map robot]}]
  (let [dimensions [(inc (apply max (map first (keys coord-map))))
                    (inc (apply max (map second (keys coord-map))))]]
    (with-out-str
      (mx/pm (mx/emap-indexed (fn [idx _]
                                (if (= idx robot) \@
                                    (coord-map idx)))
                              (apply mx/new-matrix dimensions))))))

; Denne algoritmen gir en score til en gitt boks:

(defn gps-score [[y x :as _box]]
  (+ (* 100 y) x))

(defn update-map [coord-map new-locations move]
  (let [cleared (reduce (fn [m l]
                          (dissoc m l))
                        coord-map new-locations)]
    (reduce (fn [m l]
              (assoc m (move-to-new-position l move) (coord-map l)))
            cleared new-locations)))

; Step-funksjonen som endrer game-state gitt en bevegelse gjør følgende:
; 1. Flytt roboten
; 2. Gjør et bredde-først-søk fra den nye posisjonen hvor vi:
;     1. Ser om det som befinner seg på den nye posisjonen er en boks
;     2. Hvis stor boks, legg også til den andre delen av boksen
;     3. Utfør samme flytt på denne nye posisjonen
;     4. Gjentar steg 1 med de nye posisjonene
; 3. Sjekk om noen av de nye posisjonene traff en vegg
; 4. Hvis ingen vegg, utfør flyttet.

(defn step [{:keys [robot coord-map] :as game-state} move]
  (let [new-robot-position (move-to-new-position robot move)
        affected-boxes (fn [pos]
                         (case (coord-map pos)
                           \O [(move-to-new-position pos move)]
                           \[ [(move-to-new-position pos move)
                               (move-to-new-position pos \>)]
                           \] [(move-to-new-position pos move)
                               (move-to-new-position pos \<)]
                           []))
        new-locations (->> (utils/breadth-search affected-boxes new-robot-position)
                           (mapcat identity)
                           (filter coord-map))]
    (if (some (comp #{\#} coord-map) new-locations)
      game-state
      (assoc game-state
             :robot new-robot-position
             :coord-map (update-map coord-map new-locations move)))))

^{:kind/hiccup true :kindly/hide-code true}
(let [game-state (:game-state (map-and-moves "...#...
...O...
...@...
......#"))]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"}}
   [:h3 {:style {:grid-column "1 / span 2"}} "Flytt opp mot vegg, ingen effekt"]
   [:code [:pre (print-board game-state)]]
   [:code [:pre (print-board (step game-state \^))]]])

; Hvis vi flytter en boks som ikke står mot en vegg:

^{:kind/hiccup true :kindly/hide-code true}
(let [game-state (:game-state (map-and-moves "...#...
.......
...O...
...@..#"))]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"}}
   [:h3 {:style {:grid-column "1 / span 2"}} "Flytt opp, flytter også boks"]
   [:code [:pre (print-board game-state)]]
   [:code [:pre (print-board (step game-state \^))]]])

; Hvis vi flytter flere bokser som ikke står mot en vegg:

^{:kind/hiccup true :kindly/hide-code true}
(let [game-state (:game-state (map-and-moves "...#...
.......
...O...
...O...
...@..#"))]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"}}
   [:h3 {:style {:grid-column "1 / span 2"}} "Flytt opp, flytter alle bokser"]
   [:code [:pre (print-board game-state)]]
   [:code [:pre (print-board (step game-state \^))]]])

(defn solve [{:keys [game-state moves]}]
  (->> (reduce step game-state moves)
       :coord-map
       (medley/filter-vals #{\O \[})
       keys
       (map gps-score)
       (reduce +)))

; Da kan vi løse del 1:

(solve (map-and-moves test-data-small))

(solve (map-and-moves test-data-large))

(delay (solve (map-and-moves (input/get-input 2024 15))))

; ## Del 2

; I del 2 har alt unntatt roboten blitt doblet i størrelse, som vil fører
; til at algoritmene vi laget i del 1 ikke fungerer lenger, siden vi nå må
; dytte bokser med bredde = 2

; Vi må starte med å utvide kartet:

(defn expand-board [input]
  (str/escape input {\# "##" \O "[]" \. ".." \@ "@."}))

^{:kind/hiccup true :kindly/hide-code true}
[:div {:style {:display "grid" :grid-template-columns "1fr 1fr"}}
 [:h3 {:style {:grid-column "1 / span 2"}} "Utvidet testdata"]
 [:code [:pre test-data-small]]
 [:code [:pre (expand-board test-data-small)]]]

; Vi kan parse kartet på omtrent samme måte som tidligere, selv om boksene
; er bredere, så lenge `map-and-moves` tolker både `[]` og `O` som bokser.

; Eksempel på flytting av flere bokser i et diamant-mønster:

^{:kind/hiccup true :kindly/hide-code true}
(let [game-state (:game-state (map-and-moves "...#...
.......
...[]...
..[][]..
...[]...
...@..#"))]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"}}
   [:h3 {:style {:grid-column "1 / span 2"}} "Flytt opp, flytter alle bokser"]
   [:code [:pre (print-board game-state)]]
   [:code [:pre (print-board (step game-state \^))]]])

; Bredde-først-søket må ikke fange opp nabo-bokser som ikke skal bli flyttet:

^{:kind/hiccup true :kindly/hide-code true}
(let [game-state (:game-state (map-and-moves "...#...
.......
...[]...
..[][]..
..@[]...
......#"))]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"}}
   [:h3 {:style {:grid-column "1 / span 2"}} "Flytt opp, flytter ikke boksene til høyre"]
   [:code [:pre (print-board game-state)]]
   [:code [:pre (print-board (step game-state \^))]]])

^{:kind/hiccup true :kindly/hide-code true}
(let [game-state (:game-state (map-and-moves "...#...
.......
...[]...
.@[][]..
...[]...
......#"))]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"}}
   [:h3 {:style {:grid-column "1 / span 2"}} "Flytt høyre, flytter de midterste boksene"]
   [:code [:pre (print-board game-state)]]
   [:code [:pre (print-board (step game-state \>))]]])

; Når vi er sikre på at algoritmen fungerer kan vi løse del 2:

(solve (map-and-moves (expand-board test-data-large)))

(delay (solve (map-and-moves (expand-board (input/get-input 2024 15)))))
