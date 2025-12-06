^:kindly/hide-code
(ns y2025.d06 
  (:require
    [advent-of-code-clj.utils :as utils]
    [advent-of-code-clj.input :as input]))

; # 2025, dag 6

; ## Del 1

; Vi skal summere opp regnestykker som er skrevet på en ukonvensjonell
; måte. Inputten består av flere kolonner med regnestykker, operandene
; ligger øverst i kolonnen, og operatoren er i bunn:

(def test-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

; Vi skriver en parser som henter ut regnestykkene til en liste, med operator først
; og operandene i tekstform etter (forberede for del 2):

(defn parse-input [input]
  (let [lines (String/.split input "\n")
        digits-or-math-symbol #"(\+|\*|\d+)"
        numbers-and-symbols (mapv #(map first (re-seq digits-or-math-symbol %)) lines)
        numbers (apply mapv vector (butlast numbers-and-symbols))
        symbols (map (comp resolve symbol) (last numbers-and-symbols))] 
    (map (fn [s nums]
           (cons s nums)) symbols numbers)))

(parse-input test-input)

; Del 1 løses da ved å konvertere til tall og evaluere listene, ferdig formatert som clojure-kode:

(defn part-1 [input]
  (let [problems (parse-input input)
        with-numbers (map (fn [[f & args]]
                            (cons f (map parse-long args))) problems)] 
    (reduce + (map eval with-numbers))))

(= 4277556 (part-1 test-input))

(= 6417439773370 (part-1 (input/get-input 2025 6)))

; ## Del 2

; I del 2 innser vi at tallene egentlig skal leses som en transponert matrise. Feks skal første regnestykke
; (123, 45, 6) egentlig leses som (356, 24, 1). Så vi må utføre en transponering.

(defn transpose-numbers [number-string-xs]
  (let [char-seq (mapv (comp vec reverse seq) number-string-xs)
        max-count (apply max (map count char-seq))]
    (loop [transposed-numbers []
           idx 0]
      (if (= idx max-count)
        (map (fn [tx]
               (let [joined-number (String/join "" (map str tx))]
                 (parse-long joined-number))) transposed-numbers)
        (recur (conj transposed-numbers (keep #(get % idx nil) char-seq))
               (inc idx))))))

(= [356 24 1] (transpose-numbers ["123" "45" "6"]))

(= [356 24 1] (transpose-numbers ["51" "387" "215"]))

; Transponering løser ikke problemet, fordi noen kolonner kan ha siffer som er høyrejustert eller venstrejustert!
; Siden vi er usikker på hvordan vi løser det, er det på tide å forsøke en annen strategi, nemlig å transponere
; hele inputten:

(defn transpose-input [input]
  (let [matrix (utils/text->matrix input)
        tx (apply mapv vector matrix)
        split (remove #(= 1 (count %)) (partition-by #(every? #{\space} %) tx))]
    (map #(apply mapv vector %) split)))

(transpose-input test-input)

; Med transponert input kan vi gjøre ca samme prosess som når vi transponerte tall:

(defn generate-math-form [transposed-matrix]
  (let [max-idx (count (first transposed-matrix))
        operator (-> transposed-matrix last first str symbol resolve)
        operand-grid (butlast transposed-matrix)]
    (loop [transposed-numbers []
           current-idx 0]
      (if (= max-idx current-idx)
        (cons operator (map (fn [tx]
                              (let [joined-number (String/join "" (map str tx))]
                                (parse-long joined-number))) transposed-numbers))
        (recur
          (conj transposed-numbers (keep (fn [line] 
                                           (let [val (get line current-idx)]
                                             (when (not= \space val)
                                               val))) 
                                         operand-grid)) 
          (inc current-idx))))))

(->> test-input transpose-input (map generate-math-form))

(defn part-2 [input]
  (let [transposed-input (transpose-input input)
        math-forms (map generate-math-form transposed-input)]
    (reduce + (map eval math-forms))))

(= 3263827 (part-2 test-input))

(part-2 (input/get-input 2025 6))