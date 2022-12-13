(ns advent-of-code-clj.y2020.d01
  (:require [clojure.string :as str]))

(comment
  (def liste-av-tall (map parse-long
                          (str/split-lines (slurp "input/y2020/01.txt"))))

  ; Multipliser to tall som i sum er lik 2020
  (loop [[x & resten :as listen] liste-av-tall]
    (if (nil? listen)
      (throw (ex-info "Gikk tom for tall" {}))
      (if-let [y (->> resten
                      (filter #(= 2020 (+ x %)))
                      first)]
        (* x y)
        (recur resten))))

  ; Multipliser tre tall som i sum er lik 2020
  (loop [[x & rest-1 :as liste] liste-av-tall]
    (if (nil? liste)
      (throw (ex-info "Gikk tom for tall" {}))
      (if-let [multiplert (loop [[y & rest-2 :as liste] rest-1]
                            (if (nil? liste)
                              nil
                              (if-let [z (->> rest-2
                                              (filter #(= 2020 (+ x y %)))
                                              first)]
                                (* x y z)
                                (recur rest-2))))]
        multiplert
        (recur rest-1))))

  ; Multipliser to tall som i sum er lik 2020
  ; NB: Fungerer ikke; x og y kan være samme nummer
  (first (for [x liste-av-tall
               y (drop 1 liste-av-tall)
               :when (= 2020 (+ x y))]
           (* x y))))