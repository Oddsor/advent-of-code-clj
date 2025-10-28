(ns clay
  (:require
    [clojure.string :as str]
    [portal.api :as p]
    [scicloj.clay.v2.api :as clay])
  (:import
    [java.io File]))

(defn- add-title [html]
  (if-let [title (second (first (re-seq #"<h1.*?>(.*?)</h1>" html)))]
    (.replaceFirst html
                   "<title>.*?</title>" (str "<title>" title "</title>"))
    html))

(defn- add-backlink [html]
  (if (String/.contains html "source: src/index.clj")
    html
    (str/replace-first html #"<h1(.*?)>" "<h1$1><a href=\"index.html\">&larr;</a> ")))

(defn build-notebooks! [_]
  (set! *warn-on-reflection* true)
  (clay/make!
    {:source-path (conj
                    (mapv str (filter File/.isFile (tree-seq File/.isDirectory File/.listFiles (File. "notebooks"))))
                    "src/index.clj")
     :title "Advent of Code"
     :base-target-path "_site/new"
     :show false
     :live-reload false
     :post-process (comp add-backlink add-title)})
  (p/stop))
