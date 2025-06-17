(ns y2024.d06
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.utils :as utils]
    [medley.core :as medley])
  (:import
    (clojure.lang IPersistentVector)
    [java.util HashSet]))

;; # Dag 6 - Guard Gallivant

;; ## Del 1

;; Del 1 ser ut til å være ganske grei; gitt en start-posisjon, finn alle
;; posisjoner som vakten dekker hvis den går fremover og snur til høyre hver
;; gang den treffer et hinder (`#`)

(def test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

;; Vi parser input til et koordinat-map. For å finne start-posisjonen
;; henter vi koordinatet som har verdien `^`:

(defn find-starting-pos [coord-map]
  (key (medley/find-first #(= \^ (val %)) coord-map)))

;; Retningen defineres av en retningsvektor, som endrer seg slik at ny
;; retning til høyre fra forrige retning; representert som et hashmap:

(def new-dir {:u :r
              :r :d
              :d :l
              :l :u})

(defn move [pos dir]
  ;; Raskere enn destrukturering (https://blog.redplanetlabs.com/2020/09/02/clojure-faster/#destructuring)
  (let [x (IPersistentVector/.nth pos 0)
        y (IPersistentVector/.nth pos 1)]
    (case dir
      :u [x (dec y)]
      :r [(inc x) y]
      :d [x (inc y)]
      :l [(dec x) y])))

;; Nå simulerer vi bevegelsen til vakten ved å flytte posisjonen ett steg
;; hver runde. Alle besøkte posisjoner lagres i et sett.

;; Ganske naiv løsning, men den kjører på greie ~75ms for inputten:

(defn part-1 [input]
  (let [coord-map (->> input
                       utils/text->matrix
                       utils/coord-map)]
    (loop [pos (find-starting-pos coord-map)
           dir :u
           visited-nodes #{}]
      (let [new-pos (move pos dir)
            nvisited (conj visited-nodes pos)]
        (case (coord-map new-pos)
          (\. \^) (recur new-pos dir nvisited)
          \# (recur pos (new-dir dir) nvisited)
          nil (count nvisited))))))

(part-1 test-input)

(part-1 (input/get-input 2024 6))

;; ## Part 2

;; I del 2 skal vi finne ut hvor mange måter vakten kan bli satt fast
;; i en uendelig loop på.

;; Den naive løsningen ville vært å plassere et hinder på alle besøkte
;; noder, og deretter kjøre simuleringen på nytt for å oppdage en loop,
;; men vi mangler en god måte å oppdage loops på. Vi starter der:

(defn find-path
  ([coord-map] (find-path coord-map (find-starting-pos coord-map)))
  ([coord-map starting-pos]
   (let [visited (HashSet.)]
     (loop [pos starting-pos
            dir :u]
       (if (HashSet/.contains visited [pos dir])
         [:loop visited]
         (let [new-pos (move pos dir)]
           (HashSet/.add visited [pos dir])
           (case (coord-map new-pos)
             (\. \^) (recur new-pos dir)
             \# (recur pos (new-dir dir))
             nil [:end visited])))))))

(find-path (-> test-input utils/text->matrix utils/coord-map))

(defn yields-loop? [coord-map starting-pos obstacle-position]
  (let [coord-map-with-obstacle (assoc coord-map obstacle-position \#)
        [result _] (find-path coord-map-with-obstacle starting-pos)]
    (= :loop result)))

(defn part-2 [input]
  (let [coord-map (-> input utils/text->matrix utils/coord-map)
        [_ original-path] (find-path coord-map)
        original-coordinates (into #{}
                                   (map first)
                                   original-path)
        starting-pos (find-starting-pos coord-map)
        coordinates-minus-starting-pos (disj original-coordinates
                                             starting-pos)]
    (->> coordinates-minus-starting-pos
         (pmap #(yields-loop? coord-map starting-pos %))
         (filter true?)
         count)))

(part-2 test-input)

;; På ekte input tar løsningen ~1.9 sekunder med parallellisering, som
;; er litt langsommere enn jeg håpet. Her kan vi nok optimalisere enda mer

(delay (part-2 (input/get-input 2024 6)))
