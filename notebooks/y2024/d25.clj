(ns y2024.d25
  (:require
    [advent-of-code-clj.input :as input]
    [advent-of-code-clj.matrix :as mx]))

(def test-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(def example-key ".....
.....
.....
#....
#.#..
#.#.#
#####")

(defn parse-key [input]
  (map (fn [line]
         (dec (count (filter #{\.} line))))
       (mx/transpose (mx/text->matrix input))))
(defn parse-lock [input]
  (map (fn [line]
         (dec (count (filter #{\#} line))))
       (mx/transpose (mx/text->matrix input))))

(defn parse [input]
  (let [lock-or-key (String/.split input "\n\n")
        lock (filter #(String/.startsWith % "#####") lock-or-key)
        key (remove #(String/.startsWith % "#####") lock-or-key)]
    {:keys (map parse-key key)
     :locks (map parse-lock lock)}))

(defn part-1 [input]
  (let [{:keys [locks keys]} (parse input)]
    (count (for [key keys
                 lock locks
                 :let [subbed (map - key lock)]
                 :when (every? nat-int? subbed)]
             subbed))))

(part-1 test-input)

(part-1 (input/get-input 2024 25))
