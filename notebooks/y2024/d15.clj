(ns y2024.d15
  (:require
   [advent-of-code-clj.utils :as utils]
   [advent-of-code-clj.input :as input]
   [clojure.core.matrix :as mx]))

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

(def dir-vector {\< [0 -1]
                 \v [1 0]
                 \^ [-1 0]
                 \> [0 1]})

(defn map-and-moves [input]
  (let [[game-map-str moves-str] (String/.split input "\n\n")
        game-matrix (utils/text->matrix game-map-str)
        coord-map (utils/coord-map-fixed game-matrix)
        grouped (group-by val coord-map)]
    {:game-state {:walls (->> (grouped \#) (map key) set)
                  :boxes (->> (grouped \O) (map key) set)
                  :pos (->> (grouped \@) first key)}
     :moves (keep dir-vector moves-str)}))

(map-and-moves test-data-small)

(defn push-boxes [walls boxes pos move]
  (let [path (seq (iteration (fn [x] (mapv + x move))
                             :initk pos
                             :somef #(not (walls %))))
        box-positions (map boxes path)
        boxes-to-move (set (take-while some? box-positions))]
    (if (= (count boxes-to-move) (count path))
      nil ; No boxes could be moved
      (let [remaining-boxes (remove boxes-to-move boxes)]
        (into (set remaining-boxes) (map (fn [box] (mapv + box move)) boxes-to-move))))))

(defn print-board [{:keys [pos walls boxes]}]
  (let [dimensions [(inc (apply max (map first walls)))
                    (inc (apply max (map second walls)))]
        walls (set walls)
        boxes (set boxes)]
    (mx/pm (mx/emap-indexed (fn [idx _]
                              (cond
                                (walls idx) \#
                                (boxes idx) \O
                                (= pos idx) \@)) (apply mx/new-matrix dimensions)))))

(defn make-move [{:keys [pos walls boxes] :as game-state} move]
  (let [maybe-new-pos (mapv + pos move)]
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
       (#(do
           (print-board %)
           %))
       :boxes
       (map gps-score)
       (reduce +)))

(part-1 (map-and-moves test-data-small))

(part-1 (map-and-moves test-data-large))

(delay (part-1 (map-and-moves (input/get-input 2024 15))))
