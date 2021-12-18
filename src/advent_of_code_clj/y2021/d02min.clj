(ns advent-of-code-clj.y2021.d02min)

(def test-data "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse [text]
  (map (fn [[operator amount]]
         (case operator
           up [0 (- amount)]
           down [0 amount]
           forward [amount 0]))
       (partition 2 (read-string (str "[" text "]")))))

(defn update-position-with-aim
  [[h d a] [nh da]]
  [(+ h nh)
   (+ d (* nh a))
   (+ a da)])

(comment
  ;; Part 1
  (->> (parse (slurp "input/y2021/02.txt")) (apply map +) (apply *)))

(comment
  ;; Part 2
  (->> (parse (slurp "input/y2021/02.txt"))
       (reduce update-position-with-aim [0 0 0])
       (take 2)
       (apply *)))

;; Asserts
(assert (= [15 10]
           (->> (parse test-data) (apply map +))))
(assert (= 150
           (->> (parse test-data) (apply map +) (apply *))))
(assert (= [15 60 10]
           (->> (parse test-data)
                (reduce update-position-with-aim [0 0 0]))))
(assert (= 900
           (->> (parse test-data)
                (reduce update-position-with-aim [0 0 0])
                (take 2)
                (apply *))))

(comment
  ;; minimert
  ;; (let[x(map(fn[[o x]](case(nth(name o)0)\u[0(- x)]\d[0 x]\f[x 0]))(partition 2(read-string(str"["(slurp *in*)"]"))))a apply](prn(a *(a map + x))(a *(take 2(reduce(fn[[h d a][x y]][(+ h x)(+ d(* x a))(+ a y)])[0 0 0]x)))))
  (let [x (map (fn [[o x]]
                 (case (nth (name o) 0)
                   \u [0 (- x)]
                   \d [0 x]
                   \f [x 0]))
               (partition 2 (read-string (str "[" (slurp "input/y2021/02.txt") "]"))))
        a apply]
    (prn (a * (a map + x)) (a * (take 2 (reduce (fn [[h d a] [x y]] [(+ h x) (+ d (* x a)) (+ a y)]) [0 0 0] x)))))
  )