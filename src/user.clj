(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(comment
  ;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
  (clerk/serve! {:browse? true})
  ;; either call `clerk/show!` explicitly
  (clerk/show! "notebooks/y2022_d01.clj")
  ;; or let Clerk watch the given `:paths` for changes
  (clerk/serve! {:watch-paths ["notebooks" "src"]})

  ;; start with watcher and show filter function to enable notebook pinning
  (clerk/serve! {:watch-paths ["notebooks" "src"] :show-filter-fn #(str/starts-with? % "notebooks")}))

(defn build-notebooks! [_]
  ;; Build a html file from the given notebook notebooks.
  ;; See the docstring for more options.
  (clerk/build! {:paths (->> (io/as-file "notebooks")
                             (tree-seq #(.isDirectory ^File %) #(.listFiles ^File %))
                             (remove #(.isDirectory ^File %))
                             (map #(.toString ^File %))
                             sort)
                 :out-path "_site"}))
