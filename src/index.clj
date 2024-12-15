^:kindly/hide-code
(ns index
  (:require
   [scicloj.kindly.v4.kind :as kind])
  (:import
   [java.io File]))

; # Advent of Code!

; Gå til en løsning

^:kindly/hide-code
(kind/hiccup
 (let [matcher #"notebooks\/(y\d+)\/(d\d+).clj"]
   (->> (File. "notebooks")
        (tree-seq File/.isDirectory File/.listFiles)
        (filter File/.isFile)
        (mapv str)
        (sort)
        (mapv (fn [path]
                (let [[_ y d] (re-matches matcher path)]
                  [:li [:a {:href (str y "." d ".html")}
                        (str "Year" y ", day" d)]])))
        (cons :ul)
        vec)))
