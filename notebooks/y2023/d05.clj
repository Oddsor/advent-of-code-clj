(ns y2023.d05
  (:require [clojure.string :as str]
            [criterium.core :as crit]))

(def test-data "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn conversion-fn [chunk]
  (let [[_ & ch] (str/split-lines chunk)
        xs (mapv #(mapv parse-long (re-seq #"\d+" %)) ch)
        xfn (map (fn [[dst src rng]] (fn [num] (when (>= (+ src rng) num src)
                                                 (+ dst (- num src))))) xs)]
    (fn [number]
      (let [destination (first (keep #(% number) xfn))]
        (or destination number)))))

(defn part-1 [data]
  (let [[seed-str seed->soil-str soil->fertilizer-str
         fertilizer->water-str water->light-str
         light->temperature-str temperature->humidity-str
         humidity->location-str] (str/split data #"\n\n")
        seeds (map parse-long (re-seq #"\d+" seed-str))
        seed->soil (conversion-fn seed->soil-str)
        soil->fertilizer (conversion-fn soil->fertilizer-str)
        fertilizer->water (conversion-fn fertilizer->water-str)
        water->light (conversion-fn water->light-str)
        light->temperature (conversion-fn light->temperature-str)
        temperature->humidity (conversion-fn temperature->humidity-str)
        humidity->location (conversion-fn humidity->location-str)]
    (->> seeds
         (map (fn [x]
                (->> x
                     seed->soil
                     soil->fertilizer
                     fertilizer->water
                     water->light
                     light->temperature
                     temperature->humidity
                     humidity->location)))
         (apply min))))

(= 35 (part-1 test-data))

(comment
  (part-1 (slurp "input/2023/d05.txt")))

(defn part-2 [data]
  (let [[seed-str & conversions] (str/split data #"\n\n")
        seed-ranges (partition 2 (map parse-long (re-seq #"\d+" seed-str)))
        seed->location (map conversion-fn conversions)]
    (->> seed-ranges
         (pmap (fn [[start rng]]
                 (let [max (+ start rng)]
                   (loop [x start
                          current-min Long/MAX_VALUE]
                     (if (> x max)
                       current-min
                       (let [loc (reduce (fn [acc sl] (sl acc))
                                         x seed->location)]
                         (recur (inc x) (min current-min loc))))))))
         (apply min))))

(= 46 (part-2 test-data))
(comment
  (part-2 (slurp "input/2023/d05.txt")))