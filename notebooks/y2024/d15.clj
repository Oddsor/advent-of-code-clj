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

(defn map-and-moves [input]
  (let [[game-map-str moves-str] (String/.split input "\n\n")
        game-matrix (utils/text->matrix game-map-str)
        coord-map (utils/coord-map-fixed game-matrix)
        grouped (group-by val coord-map)]
    {:game-state {:coord-map (assoc coord-map (->> (grouped \@) first key) \.)
                  :walls (->> (grouped \#) (map key) set)
                  :boxes (->> (concat (grouped \O) (grouped \[)) (map key) set)
                  :pos (->> (grouped \@) first key)
                  :robot (->> (grouped \@) first key)}
     :moves (filter #{\^ \> \< \v} moves-str)}))

(map-and-moves test-data-small)

(defn move-to-new-position [[old-y old-x] move]
  (case move
    \^ [(dec old-y) old-x]
    \> [old-y (inc old-x)]
    \v [(inc old-y) old-x]
    \< [old-y (dec old-x)]))

(defn push-boxes [walls boxes pos move]
  (let [path (seq (iteration (fn [x] (move-to-new-position x move))
                             :initk pos
                             :somef #(not (walls %))))
        box-positions (map boxes path)
        boxes-to-move (set (take-while some? box-positions))]
    (if (= (count boxes-to-move) (count path))
      nil ; No boxes could be moved
      (let [remaining-boxes (remove boxes-to-move boxes)]
        (into (set remaining-boxes) (map (fn [box]
                                           (move-to-new-position box move))
                                         boxes-to-move))))))

(defn print-board [{:keys [pos walls boxes]}]
  (let [dimensions [(inc (apply max (map first walls)))
                    (inc (apply max (map second walls)))]
        walls (set walls)
        boxes (set boxes)]
    (mx/pm (mx/emap-indexed (fn [idx _]
                              (cond
                                (walls idx) \#
                                (boxes idx) \O
                                (= pos idx) \@))
                            (apply mx/new-matrix dimensions)))))

(defn print-board-cm [{:keys [coord-map robot]}]
  (let [dimensions [(inc (apply max (map first (keys coord-map))))
                    (inc (apply max (map second (keys coord-map))))]]
    (mx/pm (mx/emap-indexed (fn [idx _]
                              (if (= idx robot) \@
                                  (coord-map idx)))
                            (apply mx/new-matrix dimensions)))))

(defn make-move [{:keys [pos walls boxes] :as game-state} move]
  (let [maybe-new-pos (move-to-new-position pos move)]
    (cond
      (walls maybe-new-pos) game-state
      (boxes maybe-new-pos) (if-let [pushed-boxes (push-boxes walls boxes pos move)]
                              (assoc game-state
                                     :pos maybe-new-pos
                                     :boxes pushed-boxes)
                              game-state)
      :else (assoc game-state :pos maybe-new-pos))))

(defn gps-score [[y x :as _box]]
  (+ (* 100 y) x))

(defn part-1 [{:keys [game-state moves]}]
  (->> (reduce make-move game-state moves)
       :boxes
       (map gps-score)
       (reduce +)))

(part-1 (map-and-moves test-data-small))

(part-1 (map-and-moves test-data-large))

(delay (part-1 (map-and-moves (input/get-input 2024 15))))

; ## Del 2

; I del 2 har alt unntatt roboten blitt doblet i størrelse, som vil fører
; til at algoritmene vi laget i del 1 ikke fungerer lenger, siden vi nå må
; dytte bokser med bredde = 2

; Vi må starte med å utvide kartet:

(defn expand-board [input]
  (str/escape input {\# "##" \O "[]" \. ".." \@ "@."}))

^{:kind/hiccup true}
[:code [:pre (expand-board test-data-small)]]

; Vi kan parse kartet på omtrent samme måte som tidligere, selv om boksene
; er bredere, så lenge `map-and-moves` tolker både `[` og `O` som bokser:

(map-and-moves (expand-board test-data-small))

; Vi trenger en ny algoritme for dytting av bokser. Her satt jeg fast lenge,
; så jeg så til andre løsninger for inspirasjon og fant en løsning fra
; [Zelark](https://github.com/zelark/AoC/blob/master/src/zelark/aoc_2024/day_15.clj):

; 1. Flytt roboten
; 2. Gjør et bredde-først-søk fra den nye posisjonen hvor vi:
;     1. Ser om det som befinner seg på den nye posisjonen er en boks
;     2. Hvis vertikal forflytning og en stor boks, legg til den andre delen av boksen
;     3. Hent den nye posisjonen til boksen
;     4. Gjentar steg 1 med de nye posisjonene
; 3. Sjekk om noen av de nye posisjonene er en vegg
; 4. Hvis ingen vegg, utfør flyttet.

(defn update-map [coord-map new-locations move]
  (let [cleared (reduce (fn [m l]
                          (assoc m l \.)) coord-map new-locations)]
    (reduce (fn [m l]
              (assoc m (move-to-new-position l move) (coord-map l)))
            cleared new-locations)))

(defn step [{:keys [robot coord-map] :as game-state} move]
  (let [new-robot-position (move-to-new-position robot move)
        vertical-movement? #{\^ \v}
        affected-boxes (fn [pos]
                         (case (coord-map pos)
                           \O [(move-to-new-position pos move)]
                           \[ (cond-> [(move-to-new-position pos move)]
                                (vertical-movement? move)
                                (conj (move-to-new-position pos \>)))
                           \]
                           (cond-> [(move-to-new-position pos move)]
                             (vertical-movement? move)
                             (conj (move-to-new-position pos \<)))
                           []))
        new-locations (->> (utils/breadth-search affected-boxes new-robot-position)
                           (mapcat identity)
                           (remove (comp #{\.} coord-map)))]
    (if (some (comp #{\#} coord-map) new-locations)
      game-state
      (assoc game-state
             :robot new-robot-position
             :coord-map (update-map coord-map new-locations move)))))

(defn solve [{:keys [game-state moves]}]
  (->> (reduce step game-state moves)
       :coord-map
       (medley/filter-vals #{\O \[})
       keys
       (map gps-score)
       (reduce +)))

; Den nye algoritmen løser både del 1:

(solve (map-and-moves test-data-small))

(solve (map-and-moves test-data-large))

(solve (map-and-moves (input/get-input 2024 15)))

; Og del 2:

(solve (map-and-moves (expand-board test-data-large)))

(solve (map-and-moves (expand-board (input/get-input 2024 15))))
