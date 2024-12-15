(ns clay
  (:require
   [scicloj.clay.v2.api :as clay])
  (:import
   [java.io File]))

(clay/make!
 {:source-path (conj
                (mapv str (filter File/.isFile (tree-seq File/.isDirectory File/.listFiles (File. "notebooks"))))
                "src/index.clj")
  :title "Advent of Code"
  :base-target-path "_clay"
  :hide-ui-header true})
