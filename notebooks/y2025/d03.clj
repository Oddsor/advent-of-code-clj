(ns y2025.d03 
  (:require
    [advent-of-code-clj.input :as input]))

; # 2025, dag 3

; Dårlig tid i dag, så det blir bare rett på brute-force løsning:

(def test-input "987654321111111
811111111111119
234234234234278
818181911112111")

(defn input->battery-banks [input]
  (->> (String/.split input "\n")
       (map (fn [line]
              (mapv (comp Integer/parseInt str) line)))))

(input->battery-banks test-input)

(defn all-number-combinations [bank]
  (let [max-n (count bank)]
    (for [a (range 0 (dec max-n))
          b (range (inc a) max-n)]
      (Integer/parseInt (str (bank a) (bank b))))))

(all-number-combinations [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1])

(defn jolt [bank]
  (apply max (all-number-combinations bank)))

(jolt [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1])

(defn part-1 [input]
  (->> input
       input->battery-banks
       (map jolt)
       (reduce +)))

(part-1 test-input)

(part-1 (input/get-input 2025 3))