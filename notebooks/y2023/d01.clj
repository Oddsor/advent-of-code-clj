^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d01
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; # Year 2023, day 1

;; ## Part 1

;; Very sneaky task! Part one is simple enough, and only requires that we collect
;; all digits from the strings, then make a new number from the first and last digits.

;; So given the following test-data:
(def test-data "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

;; We simply split the text into lines and find the numbers via regex, and combine the first
;; and last digit so that they combine into a two-digit number

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-digits [text] (re-seq #"\d" text))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (map find-digits (str/split-lines test-data)))

^{:nextjournal.clerk/visibility {:result :hide}}
(def first-and-last (juxt first last))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (first-and-last ["1" "5" "3" "2"]))

^{:nextjournal.clerk/visibility {:result :hide}}
(def combine-first-and-last-digit (comp parse-long str/join first-and-last find-digits))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/example
 (combine-first-and-last-digit "pqr3stu8vwx"))

;; With helpers in hand we are able to solve the problem for the test data:

(= 142 (transduce (map combine-first-and-last-digit)
                  +
                  (str/split-lines test-data)))

;; Solve for the input:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (= 56049 (transduce (map combine-first-and-last-digit)
                      +
                      (str/split-lines (slurp "input/2023/d01.txt")))))

;; ## Part 2

;; In part two we also need to find "worded numbers", aka "one", "two", "three" etc.

;; Simple enough! Given the following test data:

(def test-data-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

;; And the following conversion table:

(def string-to-num {"one" "1"
                    "two" "2"
                    "three" "3"
                    "four" "4"
                    "five" "5"
                    "six" "6"
                    "seven" "7"
                    "eight" "8"
                    "nine" "9"})

;; We can simply expand our regex to find the digits:

(def search-pattern (re-pattern (str (str/join "|" (keys string-to-num)) "|\\d")))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-digits-worded [text]
  (let [digits-possibly-worded (re-seq search-pattern text)
        digits (map (some-fn string-to-num identity) digits-possibly-worded)]
    (->> digits first-and-last str/join parse-long)))

(= 281 (transduce (map find-digits-worded) + (str/split-lines test-data-2)))

;; Great. It works! We can now run this on the input:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  ;; WRONG!
  (= 54538 (transduce (map find-digits-worded)
                      +
                      (str/split-lines (slurp "input/2023/d01.txt")))))

;; Oops! I'm apparently not correct, the answer is too high!
;; Turns out that naively using regex for this task ends up not working for strings where some worded numbers
;; overlap. For example, the string `oneeightwo` returns the numbers `[1,8]` as the first and last digit,
;; when it should've returned `[1,2]`.

;; Apparently the problem can be solved by using ["positive lookahead"](https://www.baeldung.com/java-regex-lookahead-lookbehind),
;; which will match the overlapping words "eight" and "two":
(def search-pattern-lookahead (re-pattern (str "(?=(" (str/join "|" (keys string-to-num)) "|\\d))")))

;; By using the new lookahead-pattern, we can create a new function that will find overlapping digits

^{:nextjournal.clerk/visibility {:result :hide}}
(defn find-digits-worded-overlapping [text]
  (let [digits-possibly-worded (map last (re-seq search-pattern-lookahead text))
        digits (map (fn [x] (string-to-num x x)) digits-possibly-worded)]
    (->> digits ((juxt first last)) str/join parse-long)))

;; And this time we'll get the right answer:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  ;; Correct!
  (= 54530 (transduce (map find-digits-worded-overlapping)
                      +
                      (str/split-lines (slurp "input/2023/d01.txt")))))