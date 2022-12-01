(ns advent-of-code-clj.y2021.d02)

(defn parse [text]
  (->> text
       (re-seq #"\w+")
       (partition 2)
       (map (fn [[op amount]]
              {:op (keyword op)
               :amount (parse-long amount)}))))

(defn update-position
  [{:keys [depth h-pos] :as state} {:keys [op amount]}]
  (merge state (condp = op
                 :down {:depth (+ depth amount)}
                 :up {:depth (- depth amount)}
                 :forward {:h-pos (+ h-pos amount)})))

(defn navigate-1 [ops]
  (reduce update-position {:depth 0 :h-pos 0} ops))

(defn update-position-with-aim
  [{:keys [aim depth h-pos] :as state} {:keys [op amount]}]
  (merge state (condp = op
                 :down {:aim (+ aim amount)}
                 :up {:aim (- aim amount)}
                 :forward {:h-pos (+ h-pos amount)
                           :depth (+ depth (* amount aim))})))

(defn navigate-2 [ops]
  (reduce update-position-with-aim {:depth 0 :h-pos 0 :aim 0} ops))

(defn multiply-positions [{:keys [depth h-pos] :as _state}]
  (* depth h-pos))

(comment
  ;; Part 1
  (multiply-positions (navigate-1 (parse (slurp "input/y2021/02.txt"))))
  ;; Part 2
  (multiply-positions (navigate-2 (parse (slurp "input/y2021/02.txt")))))