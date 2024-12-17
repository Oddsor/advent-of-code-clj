(ns clay
  (:require
   [portal.api :as p]
   [scicloj.clay.v2.api :as clay])
  (:import
   [java.io File]))

(defn build-notebooks! [_]
  (clay/make!
   {:source-path (conj
                  (mapv str (filter File/.isFile (tree-seq File/.isDirectory File/.listFiles (File. "notebooks"))))
                  "src/index.clj")
    :title "Advent of Code"
    :base-target-path "_clay"
    :hide-ui-header true
    :show false
    :live-reload false
    :post-process (fn [html]
                    (when-let [title (second (first (re-seq #"<h1.*?>(.*?)</h1>" html)))]
                      (.replaceFirst html
                                     "<title>.*?</title>" (str "<title>" title "</title>"))))})
  (p/stop))
