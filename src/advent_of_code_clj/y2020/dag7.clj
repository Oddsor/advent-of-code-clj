(ns advent-of-code-clj.y2020.dag7
  (:require [clojure.string :as str]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [com.rpl.specter :as s]
            [datascript.core :as d]
            [clojure.walk :as walk]))

(def test-data "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn singular-name [text]
  (if (str/ends-with? text "s")
    (str/replace text #"s$" "")
    text))

(defn bagvec [bagnum]
  (let [[bagcount-str bagname] (str/split bagnum #"\s" 2)
        bagcount (try (Integer/parseInt bagcount-str) (catch Exception _ nil))]
    (if bagcount
      (repeat bagcount (singular-name bagname))
      [])))

(defn mapize [text]
  (let [[bag-name contains-str] (str/split text #" contain ")]
    {:bag (singular-name bag-name)
     :contains (mapcat (comp bagvec str/trim)
                       (str/split contains-str #", "))}))

(defn prepare-string [text]
  (str/split-lines (str/replace text #"\." "")))

(defn parse [text]
  (map mapize (prepare-string text)))

(defn bags-that-hold-shiny [bagmaps]
  (let [schema {:contains {:db/cardinality :db.cardinality/many
                           :db.type :db.type/ref}
                :bag {:db/unique :db.unique/identity}}
        conn (d/create-conn schema)]
    (d/transact! conn (s/transform
                       [s/ALL :contains s/ALL]
                       (fn [x]
                         {:bag x})
                       bagmaps))
    (d/q
     '[:find ?n
       :in $ %
       :where [?b :bag "shiny gold bag"]
       (fits-bag ?e ?b)
       [?e :bag ?n]]
     @conn
     '[[(fits-bag ?e1 ?e2)
        [?e1 :contains ?e2]]
       [(fits-bag ?e1 ?e2)
        [?e1 :contains ?c]
        (fits-bag ?c ?e2)]])))

(assert (= 4 (count (bags-that-hold-shiny (parse test-data)))))

(defn bags-that-shiny-holds [bagmaps]
  (let [mapped (into {} (map (juxt :bag identity))
                     bagmaps)]
    (loop [gold-bag (mapped "shiny gold bag")
           recs 0]
      (let [modified (s/transform (s/walker (every-pred
                                             coll?
                                             (comp #{:contains} first)
                                             (comp (partial every? string?)
                                                   second)))
                                  (fn [[k v]]
                                    [k (map mapped v)])
                                  gold-bag)]
        (when (> recs 100)
          (throw (ex-info "Too many recursions" {})))
        (if (= modified gold-bag)
          modified
          (recur modified (inc recs)))))))

(defn count-bags-that-shiny-holds [bagmaps]
  (let [y (volatile! -1)]
    (walk/postwalk (fn [x]
                     (when (map? x)
                       (vswap! y inc))
                     x)
                   (bags-that-shiny-holds bagmaps))
    @y))

(assert (= 32 (count-bags-that-shiny-holds (parse test-data))))

(comment
  (count (bags-that-hold-shiny (parse (slurp "input/y2020/day7-input.txt"))))
  (count-bags-that-shiny-holds (parse (slurp "input/y2020/day7-input.txt")))
  )

(comment
  ;; Forsøk med Meander
  ;; For treg til å brukes på det virkelige datasettet

  (def recursively-built-bags (let [bags (parse test-data)
                                    bags-by-name (into {} (map (juxt :bag identity) bags))]
                                ((r/until
                                  =
                                  (r/bottom-up
                                   (r/attempt
                                    (r/rewrite
                                     ((m/pred string? !bagnames) ...)
                                     ~(map bags-by-name !bagnames)))))
                                 bags)))

  (count (filterv (fn [bag]
                    (let [counter (volatile! 0)]
                      (walk/postwalk (fn [x]
                                       (when (and (map-entry? x)
                                                  (= "shiny gold bag" (val x)))
                                         (vswap! counter inc))
                                       x)
                                     bag)
                      (> @counter 0)))
                  (remove (comp #{"shiny gold bag"} :bag)
                          recursively-built-bags)))
  ;;
  )