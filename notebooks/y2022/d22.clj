^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns y2022.d22
  (:require [advent-of-code-clj.utils :refer [split-newline text->matrix]]
            [nextjournal.clerk :as clerk]))

;; # Year 2022, day 22

;; ## Part 1

;; In this exercise, we are given a map-layout and a series of operations. The dots (.) indicate an open space,
;; while hash-symbols (#) indicate a wall. When the character moves out of bounds (whitespace), they will "wrap around"
;; to the first available space on the other end of the map. So for example, going north from the top will make the
;; character pop up on the southern side of the map.

;; The last part of the dataset indicates the movement of the character. Numbers represent the number of steps forward,
;; while L/R represents turning to the left or right.

;; The character starts at the first available space at the top of the map, facing right.

;; The test dataset looks like this:

(def test-data "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

;; Parse the map and operations to create a game state. The starting direction (:direction) is always to the right,
;; the starting position is the first available open space at the top. We collect the walls and open spaces
;; into open sets so that it is easy to look up positions when moving around.

(def data-chunks (split-newline test-data))

;; The "map" is parsed into a set of walls and open spaces:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-map [cube-map-string]
  (let [mapvec (text->matrix cube-map-string)
        walls (set (for [y (range (count mapvec))
                         x (range (apply max (map count mapvec)))
                         :when (= \# (get-in mapvec [y x]))]
                     [x y]))
        open-spaces (set (for [y (range (count mapvec))
                               x (range (apply max (map count mapvec)))
                               :when (= \. (get-in mapvec [y x]))]
                           [x y]))]
    {:walls walls
     :open-spaces open-spaces
     :all-positions (into walls open-spaces)}))

(def map-coords (-> data-chunks first parse-map))

;; The movements are parsed to a sequence:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-movements [movement-string]
  (->> movement-string
       (re-seq #"[LR]|\d+")
       (mapv (fn [x]
               (if-let [xn (parse-long x)]
                 xn
                 ({"R" :R "L" :L} x))))))

(def movements (-> data-chunks second parse-movements))

;; We use the parsed map to find the starting position:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-starting-position [{:keys [open-spaces] :as _map-coords}]
  {:coord (->> open-spaces
               (filter (fn [[x y]] (zero? y)))
               (sort-by first)
               first)
   :direction :R})

(def starting-position (find-starting-position map-coords))

;; Next, some helpers. Make a lookup-map for deciding which direction the character should face.

(def update-direction {[:R :R] :D
                       [:R :L] :U
                       [:L :R] :U
                       [:L :L] :D
                       [:U :L] :L
                       [:U :R] :R
                       [:D :R] :L
                       [:D :L] :R})

;; And a "direction-to-movement"-vector:

(def direction-vector
  {:U [0 -1]
   :D [0 1]
   :L [-1 0]
   :R [1 0]})

;; And a function for moving one step in a direction:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn move-in-direction [coord direction]
  (mapv + coord (direction-vector direction)))

;; If we step outside the map (new position is not a wall or open space),
;; we need to "wrap around" to the other side of the map:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn rebound-position [{:keys [all-positions]} direction pos]
  (let [[x y] pos]
    (case direction
      :U [x (apply max (map second (filter (comp #{x} first) all-positions)))]
      :D [x (apply min (map second (filter (comp #{x} first) all-positions)))]
      :L [(apply max (map first (filter (comp #{y} second) all-positions))) y]
      :R [(apply min (map first (filter (comp #{y} second) all-positions))) y])))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (rebound-position map-coords :R [11 5])
 (rebound-position map-coords :L [-1 5])
 (rebound-position map-coords :D [5 7])
 (rebound-position map-coords :U [5 3]))

;; Given a map, direction, position and distance, we can move the character,
;; while wrapping around the map if necessary:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn update-position-wrapping
  [{:keys [open-spaces walls] :as map-coords} distance {position :coord direction :direction}]
  {:coord (nth (iterate (fn [pos]
                          (let [new-pos (move-in-direction pos direction)]
                            (cond
                              (walls new-pos) pos
                              (not (open-spaces new-pos))
                              (let [rebounded (rebound-position map-coords direction pos)]
                                (if (walls rebounded) pos rebounded))
                              :else new-pos)))
                        position)
               distance)
   :direction direction})

;; Given an operation, we can move or turn the character:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn make-move
  ([map-coords position operation]
   (make-move update-position-wrapping map-coords position operation))
  ([position-update-fn map-coords position operation]
   (if (keyword? operation)
     (update position :direction #(update-direction [% operation]))
     (position-update-fn map-coords operation position))))

;; The following algorithm calculates the "value" of the character's direction and position:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn calculate [{[x y] :coord d :direction}]
  (+ (* 1000 (inc y))
     (* 4 (inc x))
     ({:R 0 :D 1 :L 2 :U 3} d)))

;; We're finally able to solve part-1 for the test data:

(->> (reduce (partial make-move map-coords) starting-position movements)
     calculate
     (= 6032))

;; Knowing that it works, we can solve part-1 for our input:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (let [data (slurp "input/2022/22.txt")
        chunks (split-newline data)
        map-coords (parse-map (first chunks))
        movements (parse-movements (second chunks))
        starting-position (find-starting-position map-coords)]
    (-> (reduce (partial make-move map-coords) starting-position movements)
        calculate
        (= 123046))))

;; ## Part 2

;; For part 2, we realize that the final position is incorrect because the map is actually a cube!

;; This means that the wrapping-logic no longer works, and we need to think of another way to update the character's position.

;; For example, in the test data set, if the character walks off the middle towards the right, they will end up facing downwards
;; in the bottom right quadrant near the top.

;; Given a map, we need to figure out how to travel around the folded cube. For example, going down in position 3,7 will make
;; the character pop back onto the map at the bottom, facing upwards.

;; Since we don't know how to make a general solution, we can just manually map the transitions
;; from one quadrant to the next. We also need to note if the faces of the cube are inverted relative
;; to eachother:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn manual-quadrant-mapping-testdata [[x y] dir]
  (case [[x y] dir]
    [[2 1] :R] {:nq [3 2] :nd :D :invert true}
    [[2 2] :D] {:nq [0 1] :nd :U :invert true}
    [[1 1] :U] {:nq [2 0] :nd :R}))

;; Then we need to add helpers that lets us move from one face to the other of the cube.
;; First we need to analyze the map, finding the quadrant-size (4x4 for the test set), and the dimensions.

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-dimensions [{:keys [all-positions] :as map-coords}]
  (let [length-y (inc (apply max (map second all-positions)))
        length-x (inc (apply max (map first all-positions)))]
    {:length-y length-y
     :length-x length-x
     :quadrant-size (/ (max length-x length-y) 4)}))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (find-dimensions map-coords))

;; Next we need to reduce the map into quadrants

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-quadrants [{:keys [all-positions] :as map-coords}]
  (let [{:keys [length-y length-x quadrant-size]} (find-dimensions map-coords)]
    (for [y (range (/ length-y quadrant-size))
          x (range (/ length-x quadrant-size))
          :when (all-positions [(* x quadrant-size) (* y quadrant-size)])]
      [x y])))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (find-quadrants map-coords))

;; Now we have our quadrants. Next we need to know how to place a given coordinate within a quadrant:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn position->quadrant [{:keys [quadrant-size]} [x y]]
  [(int (/ x quadrant-size))  (int (/ y quadrant-size))])

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (position->quadrant (find-dimensions map-coords) [0 0])
 (position->quadrant (find-dimensions map-coords) [7 5])
 (position->quadrant (find-dimensions map-coords) [3 5])
 (position->quadrant (find-dimensions map-coords) [11 15])
 (position->quadrant (find-dimensions map-coords) [15 15]))

;; Now we have the tools needed to move a character to another quadrant. But we don't know **where** along the quadrant
;; the character should appear.

;; We need to find the offset when a character leaves a quadrant by combining the position and direction:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-offset [quadrant-size [x y] direction]
  (case direction
    (:L :R) (mod y quadrant-size)
    (:U :D) (mod x quadrant-size)))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (find-offset 4 [2 3] :R)
 (find-offset 4 [2 3] :D)
 (find-offset 4 [6 7] :L)
 (find-offset 4 [6 7] :U))

;; We should now be able to calculate the new position for a character, given a map, position and direction

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-new-position [transition-fn quadrant-size position direction]
  (let [position-in-quadrant (position->quadrant {:quadrant-size quadrant-size} position)
        next-quadrant (transition-fn position-in-quadrant direction)
        {new-direction :nd new-quadrant :nq invert :invert} next-quadrant
        offset (let [off (find-offset quadrant-size position direction)]
                 (if invert
                   (- (dec quadrant-size) off)
                   off))
        [qx qy] new-quadrant]
    {:coord (case new-direction
              :U [(+ (* qx quadrant-size) offset) (dec (* (inc qy) quadrant-size))]
              :D [(+ (* qx quadrant-size) offset) (* qy quadrant-size)]
              :L [(dec (* (inc qx) quadrant-size)) (+ (* qy quadrant-size) offset)]
              :R [(* qx quadrant-size) (+ (* qy quadrant-size) offset)])
     :direction new-direction}))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (find-new-position manual-quadrant-mapping-testdata 4 [11 11] :D)
 (find-new-position manual-quadrant-mapping-testdata 4 [5 7] :U))

;; Phew! Finally we can use our new functions to make a new movement-algorithm, and run the example:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn update-position-cube
  [transition-fn {:keys [open-spaces walls] :as map-coords} distance pos]
  (let [{:keys [quadrant-size]} (find-dimensions map-coords)]
    (nth (iterate (fn [pos]
                    (let [new-coordinate (move-in-direction (:coord pos) (:direction pos))]
                      (cond
                        (walls new-coordinate) pos
                        (not (open-spaces new-coordinate))
                        (let [rebounded (find-new-position transition-fn quadrant-size (:coord pos) (:direction pos))]
                          (if (walls (:coord rebounded)) pos
                              rebounded))
                        :else (assoc pos :coord new-coordinate))))
                  pos)
         distance)))

(->> (reduce (partial make-move (partial update-position-cube manual-quadrant-mapping-testdata) map-coords) starting-position movements)
     calculate
     (= 5031))

;; Now that we have a working solution for the test dataset, we need
;; to solve for the input. But in order to do that, we need to manually
;; add some transitions for the input data:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn manual-quadrant-mapping-input [[x y] dir]
  (case [[x y] dir]
    [[1 0] :L] {:nq [0 2] :nd :R :invert true}
    [[0 2] :L] {:nq [1 0] :nd :R :invert true}
    [[1 0] :U] {:nq [0 3] :nd :R}
    [[0 3] :L] {:nq [1 0] :nd :D}
    [[0 3] :R] {:nq [1 2] :nd :U}
    [[1 2] :D] {:nq [0 3] :nd :L}
    [[1 2] :R] {:nq [2 0] :nd :L :invert true}
    [[2 0] :R] {:nq [1 2] :nd :L :invert true}
    [[2 0] :D] {:nq [1 1] :nd :L}
    [[1 1] :R] {:nq [2 0] :nd :U}
    [[2 0] :U] {:nq [0 3] :nd :U}
    [[0 3] :D] {:nq [2 0] :nd :D}
    [[1 1] :L] {:nq [0 2] :nd :D}
    [[0 2] :U] {:nq [1 1] :nd :R}))

;; Which leaves us with the solution:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (let [data (slurp "input/2022/22.txt")
        chunks (split-newline data)
        map-coords (parse-map (first chunks))
        movements (parse-movements (second chunks))
        starting-position (find-starting-position map-coords)]
    (->> (reduce (partial make-move (partial update-position-cube manual-quadrant-mapping-input) map-coords) starting-position movements)
         calculate
         (= 195032))))
