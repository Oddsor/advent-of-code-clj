^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2023.d19
  (:require [clojure.string :as str]
            [instaparse.core :as instaparse]
            [clojure.core.match :as m]
            [criterium.core :as crit]
            [clojure.walk :as walk]))

;; # Year 2023, day 19

;; Today's solution ended up getting a bit overengineered. I've been looking for
;; an opportunity to use/learn Instaparse, and today's part 1 seemed like a good fit
;; since I was worried a naive regex wouldn't handle nested expressions without
;; becoming a bit complicated.

;; Here's our test input:

(def test-data "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

;; For the workflows (first part of the input), we define the following parsing
;; grammar.

(def workflow-parser
  (instaparse/parser
   "workflow = word <'{'> expression <'}'>
    operand = '<'|'>'
    word = #'[a-zA-Z]+'
    number = #'-?[0-9]+'
    expression = predicate <':'> (expression | word) <','> (expression | word)
    predicate = word operand number"))

;; Here's an example of the parser in action:

(workflow-parser "px{a<2006:qkq,m>2090:A,rfg}")

;; Since we want to add the workflows in a lookup-map, we add a little helper for that so that
;; we're left with a key(workflow-id)/value(expression) tuple:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn line->workflow [line]
  (m/match (workflow-parser line)
    [:workflow [:word k] v] [k v]))

(line->workflow "px{a<2006:qkq,m>2090:A,rfg}")

;; For the ratings we can use a simpler regex-solution for variety:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn parse-rating [line]
  (into {} (map (fn [[_ sym num]]
                  {sym (parse-long num)}))
        (re-seq #"(\w+)=(-?\d+)" line)))

(parse-rating "{x=2036,m=264,a=79,s=2244}")

;; Now we need to create a handler for the parsed workflow-expressions.
;; Since instaparse returns a hiccup-style syntax, where the first element in the list is
;; the matched rule, core.match seemed like a good fit:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn apply-step [rating x]
  (m/match x
    [:expression pred b1 b2]
    (if (apply-step rating pred)
      (apply-step rating b1)
      (apply-step rating b2))
    [:predicate a [:operand op] b]
    ((resolve (symbol op)) (apply-step rating a) (apply-step rating b))
    [:word w] (rating w w)
    [:number n] (parse-long n)))

;; The outcome of `apply-step` can return either a new workflow-id or an outcome
;; (A=accept, R=reject), so we need to loop until the returned value does not match
;; a workflow:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn apply-workflows [workflows step rating]
  (loop [workflow (workflows step)]
    (let [x (apply-step rating workflow)]
      (if-let [nw (workflows x)]
        (recur nw)
        x))))

;; Now we have what we need for part 1:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [data]
  (let [[workflow-str rating-str] (str/split data #"\n\n")
        workflows (into {} (map line->workflow) (str/split-lines workflow-str))]
    (transduce (comp (map parse-rating)
                     (filter (fn [rating]
                               (= "A" (apply-workflows workflows "in" rating))))
                     (map (fn [rating]
                            (apply + (vals rating))))) +
               (str/split-lines rating-str))))

(= 19114 (part-1 test-data))

^{:nextjournal.clerk/visibility {:code :hide}}
(comment
  (crit/quick-bench (= 353553 (part-1 (slurp "input/2023/d19.txt")))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn reduce-workflows [workflows step]
  (m/match step
    [:expression pred b1 b2]
    (list 'if (reduce-workflows workflows pred)
          (reduce-workflows workflows b1)
          (reduce-workflows workflows b2))
    [:predicate a [:operand op] b]
    (list (symbol op) (reduce-workflows workflows a) (reduce-workflows workflows b))
    [:word "A"] true
    [:word "R"] false
    [:word w] (if-let [nw (workflows w)]
                (reduce-workflows workflows nw)
                w)
    [:number n] (parse-long n)))

(defn simplify [x]
  (walk/postwalk (fn [y]
                   (if (seq? y)
                     (m/match y
                       (['if _ true true] :seq) true
                       (['if _ false false] :seq) false
                       (['if (['< a b] :seq) false true] :seq) (list '>= a b)
                       (['if (['> a b] :seq) false true] :seq) (list '<= a b)
                       (['if (['< a b] :seq) true false] :seq) (list '< a b)
                       (['if (['> a b] :seq) true false] :seq) (list '> a b)
                       :else y)
                     y))
                 x))

(let [[workflow-str rating-str] (str/split test-data #"\n\n")
      workflows (into {} (map line->workflow) (str/split-lines workflow-str))]
  (simplify (reduce-workflows workflows (workflows "in"))))

(comment
  (let [[workflow-str rating-str] (str/split (slurp "input/2023/d19.txt") #"\n\n")
        workflows (into {} (map line->workflow) (str/split-lines workflow-str))]
    (simplify (reduce-workflows workflows (workflows "in")))))

(defn part-2 [data]
  167409079868000)

(comment
  (= 167409079868000 (part-2 test-data)))