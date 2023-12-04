^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; # Year 2023, day 4

;; This time we are given a bunch of scratchcards, with a list of
;; winning numbers and a list of drawn numbers.
;; For example, for card 1, the matching numbers are: 83, 86, 17 and 48,
;; yielding 4 matches. Turns out the number of matches for each card is
;; the relevant metric for this task.

(def test-data "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

;; For each match, the number of points double (starting at 1 for 1 match)
;; This can be a lazy sequence of points:

(def card-points (iterate (fn [x] (if (zero? x) 1 (* x 2))) 0))

;; We only care about the amount of winning numbers, and the card number
;; So parse the card number and map it to the amount of winning numbers,
;; given by the intersection of winning numbers vs drawn numbers:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn card->winning-numbers [line]
  (let [[card-number winning-numbers drawn-numbers]
        (map #(map parse-long (re-seq #"\d+" %)) (str/split line #"[:\|]"))]
    {(first card-number)
     (count (set/intersection (set winning-numbers) (set drawn-numbers)))}))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn text->winning-numbers-by-cards [data]
  (into {} (map card->winning-numbers) (str/split-lines data)))

(text->winning-numbers-by-cards test-data)

;; ## Part 1

;; Part 1 is fairly simple; find the number of winning cards, then calculate
;; the number of points for each card. We can use the iterator above for this:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (->> data
       text->winning-numbers-by-cards
       vals
       (map #(nth card-points %))
       (apply +)))

(= 13 (part-1 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 22674 (part-1 (slurp "input/2023/d04.txt"))))

;; ## Part 2

;; Part 2 is fairly simple too (once you get rid of off-by-one-errors!). We model
;; this as a reduction over the card-numbers, and for each card we get the number of
;; winning cards, then add new cards based on that amount.

;; For example, if Card 1 matched 4 numbers, we get one of card 2,3,4 and 5.
;; This means that we can simply increase the X next cards by the current amount of cards
;; of that type.

;; To see this in action, we can generate a list of "reductions", where we see how the
;; copies are generated as we scratch each card:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn generate-copies [data]
  (let [card->matches (text->winning-numbers-by-cards data)
        max-card (->> card->matches keys (apply max))]
    (->> (sort (keys card->matches)) ;; Make sure to process cards in order
         butlast ;; Last card won't affect the count anyway
         (reductions
          (fn [acc card-num]
            (->> (zipmap (range (inc card-num)
                                (inc (min max-card (+ card-num (card->matches card-num)))))
                         (repeat (acc card-num)))
                 (merge-with + acc)))
          (zipmap (keys card->matches) (repeat 1))))))

(generate-copies test-data)

;; Part 2 is then solved by simply taking the last reduction and summing up the values:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [data]
  (->> (generate-copies data)
       last
       vals
       (apply +)))

(= 30 (part-2 test-data))
^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 5747443 (part-2 (slurp "input/2023/d04.txt"))))