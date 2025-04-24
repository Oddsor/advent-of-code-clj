^:kindly/hide-code
(ns index
  (:import
    [java.io File]))

; # Advent of Code!

; Gå til en løsning

^:kindly/hide-code
^:kind/hiccup
(let [matcher #"notebooks\/(y\d+)\/(d\d+).clj"
      file-groups (->> (File. "notebooks")
                       (tree-seq File/.isDirectory File/.listFiles)
                       (filter File/.isFile)
                       (mapv str)
                       sort
                       (group-by #(second (re-find #"y(\d{4})" %))))]
  (for [[year files] file-groups]
    (list [:h2 year]
          (->> files
               (mapv (fn [path]
                       (let [[_ y d] (re-matches matcher path)]
                         [:li [:a {:href (str y "." d ".html")}
                               (str "Dag " (subs d 1))]])))
               (cons :ul)
               vec))))
