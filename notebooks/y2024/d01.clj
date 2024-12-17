^{:nextjournal.clerk/visibility {:code :hide :result :hide}
  :kindly/hide-code true}
(ns y2024.d01
  (:require
   [nextjournal.clerk :as clerk]
   [advent-of-code-clj.input :as input]))

; # 2024, dag 1

; Denne oppgaven er relativt enkel, men ser ut til å være
; litt uvanlig i parsingen ettersom man har to lister som
; går vertikalt i inputten:

^{:nextjournal.clerk/visibility {:result :hide}}
(def test-data "3   4
4   3
2   5
1   3
3   9
3   3")

; ## Del 1

; Vi skal finne distansene mellom verdiene i hver liste,
; sortert fra lavest til høyest.

; Ettersom denne oppgaven er ganske enkel kan vi tillate oss å 
; gå noen ekstra steg og legge til et debug-bibliotek slik at vi
; kan printe stegene som utføres på testdataene:

^{:nextjournal.clerk/visibility {:result :hide}}
(require '[debux.core :as d])
^{:nextjournal.clerk/visibility {:result :hide}
  :kind/hidden true}
(d/set-debug-mode! false)

; Så kan vi starte med å få tak i listene:


^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-lists [input]
  (d/dbg
   (->> (re-seq #"\d+" input)
        (map parse-long)
        (partition 2)
        (apply mapv vector))
   :level 2))

(defmacro dbg-output [& body]
  `(let [set-debug-mode!# (requiring-resolve 'debux.core/set-debug-mode!)]
     (set-debug-mode!# true)
     (let [debug-output#
           (with-out-str ~@body)]
       (set-debug-mode!# false)

       ^:kind/hiccup [:pre debug-output#])))

(dbg-output (get-lists test-data))

; dbg-makroen lar oss inspisere hva som skjer i hvert steg
; av get-lists-funksjonen:
^{:nextjournal.clerk/visibility {:result :hide}}
(get-lists test-data)
^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/visibility {:code :hide}}
(nextjournal.clerk.viewer/with-viewer (dissoc nextjournal.clerk.viewer/string-viewer :page-size)
  (do
    (d/set-debug-mode! true)
    (let [debug-output (with-out-str (get-lists test-data))]
      (d/set-debug-mode! false)
      debug-output)))

; Når vi har listene kan vi sortere og finne differansen
; mellom hvert siffer, og summere:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn difference [a b] (abs (- a b)))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (difference 10 0)
 (difference 0 10)
 (difference 10 10))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn total-difference [input]
  (d/dbg
   (->> (get-lists input)
        (map sort)
        (apply map difference)
        (reduce +))
   :level 3))

^{:nextjournal.clerk/visibility {:result :hide}}
(total-difference test-data)
^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/visibility {:code :hide}}
(nextjournal.clerk.viewer/with-viewer (dissoc nextjournal.clerk.viewer/string-viewer :page-size)
  (do
    (d/set-debug-mode! true)
    (d/with-level 3
      (let [debug-output (with-out-str (total-difference test-data))]
        (d/set-debug-mode! false)
        debug-output))))

(total-difference (input/get-input 2024 1))

; ## Del 2

; Denne gangen finner vi antall ganger et siffer i den første
; listen dukker opp i den andre:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn total-similarity-score [input]
  (let [[list-a list-b] (get-lists input)
        freqs (frequencies list-b)
        similarity-score (fn [x] (* x (freqs x 0)))]
    (transduce (d/dbgt (map similarity-score) :level 3 :locals)
               +
               list-a)))

^{:nextjournal.clerk/visibility {:result :hide}}
(total-similarity-score test-data)
^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/visibility {:code :hide}}
(nextjournal.clerk.viewer/with-viewer (dissoc nextjournal.clerk.viewer/string-viewer :page-size)
  (do
    (d/set-debug-mode! true)
    (d/with-level 3
      (let [debug-output (with-out-str (total-similarity-score test-data))]
        (d/set-debug-mode! false)
        debug-output))))

(total-similarity-score (input/get-input 2024 1))
