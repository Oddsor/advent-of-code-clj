(ns aoc.test-utils
  (:require [malli.dev :as md]))

(defn with-instrumentation [f]
  (md/start!)
  (f)
  (md/stop!))
