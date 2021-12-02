(ns advent-of-code-clj.y2021.dag2min)

(def test-data "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse [t]
  (partition 2 (read-string (str "[" t "]"))))

(defn update-position
  [[d h] [o a]]
  (condp = o
    'down [(+ d a) h]
    'up [(- d a) h]
    'forward [d (+ h a)]))

(defn navigate-1 [o]
  (reduce update-position [0 0] o))

(defn update-position-with-aim
  [[d h a] [op m]]
  (condp = op
    'down [d h (+ a m)]
    'up [d h (- a m)]
    'forward [(+ d (* m a)) (+ h m) a]))

(defn navigate-2 [ops]
  (reduce update-position-with-aim [0 0 0] ops))

(defn multiply-positions [[d h]]
  (* d h))

(comment
  ;; Part 1
  (multiply-positions (navigate-1 (parse (slurp "input/y2021/day2-input.txt")))))

(comment
  ;; Part 2
  (multiply-positions (navigate-2 (parse (slurp "input/y2021/day2-input.txt")))))

;; Asserts
(assert (= [10 15]
           (navigate-1 (parse test-data))))
(assert (= 150 (multiply-positions
                (navigate-1 (parse test-data)))))
(assert (= [60 15 10]
           (navigate-2 (parse test-data))))
(assert (= 900 (multiply-positions
                (navigate-2 (parse test-data)))))