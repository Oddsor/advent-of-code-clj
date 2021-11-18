(ns advent-of-code-clj.dag2
  (:require [clojure.string :as str]))

(def testinput "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(def data (slurp "input/day2-input.txt"))

; Ønsker en regex som "[x]{1,3}"

(defn regel-har-riktig-antall-av-bokstav []
  (count (filter (fn [linje]
                   (let [[regel passord] (str/split linje #"\: ")
                         [antall-streng bokstav] (str/split regel #"\s")
                         [minimum maximum] (map #(Integer/parseInt %)
                                                (str/split antall-streng #"-"))]
                     (< (dec minimum)
                        (count (filter #{(first bokstav)}
                                       passord))
                        (inc maximum))))
                 (str/split-lines data))))

(defn bokstaver-er-riktig-plassert []
  (count (filter (fn [linje]
                   (let [[regel passord] (str/split linje #"\: ")
                         [pos-streng bokstav-str] (str/split regel #"\s")
                         bokstav (first bokstav-str)
                         [inkludert-posisjon ekskludert-posisjon] (map (comp dec #(Integer/parseInt %))
                                                                       (str/split pos-streng #"-"))]
                     (->> [inkludert-posisjon ekskludert-posisjon]
                          (map #(get passord % false))
                          (filter (partial = bokstav))
                          count
                          (= 1))))
                 (str/split-lines data))))