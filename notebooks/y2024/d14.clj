^:kindly/hide-code
(ns y2024.d14
  (:require
    [advent-of-code-clj.input :as input]
    [clojure.core.matrix :as mx]
    [clojure.string :as str]
    [medley.core :as medley]
    [tech.v3.dataset-api :as ds]
    [tech.v3.datatype-api :as dtype]
    [tech.v3.datatype.functional-api :as dfn]))

; # Dag 14: "Restroom Redoubt"

; ## Del 1

; Vi har en liste med roboter som har en posisjon, og en vektor som
; indikerer fart og retning.

; Målet er å simulere bevegelsen til roboter i en 2d-matrise over
; 100 "sekunder", og deretter splitte opp matrisen i kvadranter
; og kalkulere en "safety score".

(def test-data "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn robots [input]
  (eduction
    (map parse-long)
    (partition-all 2)
    (map (fn [x] (vec (reverse x))))
    (partition-all 2)
    (re-seq #"-?\d+" input)))

(robots test-data)

; Vi kan kalkulere posisjonen til roboten etter x iterasjoner
; ved å bare skalere opp retningsvektoren. Posisjonen vil være
; utenfor matrisen, så det må rettes opp senere.

(defn move-robot
  ([[posy posx] [vy vx]] [(+' posy vy) (+' posx vx)])
  ([times position velocity]
   (mx/add position (mx/scale velocity times))))

; For å få roboten tilbake i matrisen trenger vi bare å bruke modulo,
; fordi robotene "teleporterer" til den andre siden av matrisen når
; de går out of bounds.

(defn bound-robot [[dimy dimx] [posy posx]]
  [(mod posy dimy) (mod posx dimx)])

(->> (apply move-robot 100 (first (robots test-data)))
     (bound-robot [7 11]))

; Litt usikker på en mer elegant løsning for å generere et sett
; av koordinater som tilhører en kvadrant, så denne ble lang:

(defn quadrants [[dimy dimx]]
  [; quad1 
   (set (for [y (range (int (Math/floor (/ dimy 2))))
              x (range (int (Math/floor (/ dimx 2))))]
          [y x]))
   ; quad2
   (set (for [y (range (int (Math/floor (/ dimy 2))))
              x (range (int (Math/ceil (/ dimx 2))) dimx)]
          [y x]))
   ; quad3
   (set (for [y (range (int (Math/ceil (/ dimy 2))) dimy)
              x (range (int (Math/floor (/ dimx 2))))]
          [y x]))
   ; quad4
   (set (for [y (range (int (Math/ceil (/ dimy 2))) dimy)
              x (range (int (Math/ceil (/ dimx 2))) dimx)]
          [y x]))])

(defn safety-score [quadrants robot-positions]
  (transduce (map (fn [q]
                    (count (filter q robot-positions))))
             *
             quadrants))

(defn part-1 [dims input]
  (let [robot-positions (->> (robots input)
                             (map #(apply move-robot 100 %))
                             (map #(bound-robot dims %)))]
    (safety-score (quadrants dims) robot-positions)))

(part-1 [7 11] test-data)

(part-1 [103 101] (input/get-input 2024 14))

; ## Del 2

; Nå skal vi finne ut når robotene er posisjonert på en slik måte at
; de genererer et bilde av et juletre!

; Det er vanskelig å skrive en algoritme som oppdager et juletre i en
; serie med koordinater, men vi kan gjøre det manuelt dersom vi finner
; en måte å redusere antall frames vi er nødt til å se på.

; En metode vil være å kalkulere variansen i matrisen på en serie med
; tidspunkter, og så sortere på tidspunktene med lavest varians. Juletreet
; burde teoretisk sett være synlig på tidspunktet med lavest varians (?)

; Ettersom vi nå trenger å gjøre mange kalkulasjoner på x og y-aksen, så
; er det lurt å endre datamodellen til å være "column major":

(defn robots-col-major [input]
  (->>  (re-seq #"-?\d+" input)
        (map parse-long)
        (partition 4)
        (map #(zipmap [:x :y :dx :dy] %))
        (ds/->>dataset {:parser-fn :int})))

(robots-col-major test-data)

(defn move-all-robots [[dimx dimy] dataset]
  (assoc dataset
         :x (dtype/->int-array
              (dtype/emap (fn [x dx dimx]
                            (mod (+' x dx) dimx))
                          nil (:x dataset) (:dx dataset) dimx))
         :y (dtype/->int-array
              (dtype/emap (fn [y dy dimy]
                            (mod (+' y dy) dimy))
                          nil (:y dataset) (:dy dataset) dimy))))

(defn total-variance [dataset]
  (+' (dfn/variance (:x dataset))
      (dfn/variance (:y dataset))))

; Nå har vi det vi trenger for å finne løsningen, som forøvrig kjører på rundt ~3 sekunder

(defn part-2 [input]
  (let [dimensions [101 103]]
    (->> (robots-col-major input)
         (iterate #(move-all-robots dimensions %))
         (take 7000)
         (map-indexed (fn [idx dataset]
                        {:dataset dataset
                         :idx idx
                         :total-variance (total-variance dataset)}))
         (sort-by :total-variance)
         first)))

(def p2-solution (delay (time (part-2 (input/get-input 2024 14)))))

p2-solution

(defn print-board [[y x] robot-positions]
  (mx/pm (mx/emap-indexed
           (fn [index _]
             (count (filter (set [(vec index)])
                            robot-positions)))
           (mx/new-matrix y x))))

; Og slik ser brettet ut:

^{:kind/hiccup true
  :kindly/hide-code true}
(delay [:pre {:style {:line-height "0.875em"}}
        (str/replace
          (with-out-str
            (print-board [103 101]
                         (let [p (:dataset @p2-solution)]
                           (map vector (:y p) (:x p)))))
          #"\h" "")])

; Har i etterkant også lært at det er mulig å gjenbruke kode fra del 1, ved å finne robot-posisjonene som
; utgjør lavest "safety score":

(defn move-all-robots-2 [dimensions robots]
  (map (fn [[pos v]]
         [(->> (move-robot pos v)
               (bound-robot dimensions))
          v])
       robots))

(defn part-2-safety [input]
  (let [dimensions [103 101]
        quadrants (quadrants dimensions)]
    (->> (loop [game-state (robots input)
                seen #{}
                safety-scores []]
           (let [new-state (move-all-robots-2 dimensions game-state)]
             (cond (seen new-state) safety-scores
                   ; Safeguard in case we don't detect a cycle
                   (> (count safety-scores) 30000) nil
                   :else
                   (recur new-state
                          (conj seen game-state)
                          (conj safety-scores (safety-score quadrants (map first game-state)))))))
         medley/indexed
         (apply medley/least-by second))))

(delay (part-2-safety (input/get-input 2024 14)))
