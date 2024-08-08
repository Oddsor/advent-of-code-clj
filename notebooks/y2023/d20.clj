(ns y2023.d20 
  (:require [clojure.string :as str]))

(def test-data "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(let [x (into {}
              (map (fn [x]
                     (let [[k v] (str/split x #" -> ")]
                       [(-> (re-seq #"\w+" k) first keyword)
                        {:type (case (first k)
                                 \& :invert
                                 \% :flip
                                 :normal)
                         :outputs
                         (mapv keyword (re-seq #"\w+" v))}])))
              (str/split-lines test-data))]
  x)