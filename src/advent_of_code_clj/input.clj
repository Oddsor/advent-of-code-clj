(ns advent-of-code-clj.input
  (:require [taoensso.nippy :as nippy])
  (:import [io.github.cdimascio.dotenv Dotenv]))

(defonce dotenv (.. (Dotenv/configure)
                    ignoreIfMissing
                    load))

(defn get-input [year day]
  (let [password (.get dotenv "INPUT_SALT")]
    (assert (some? password)
            "Failed to get password for decrypting inputs")
    (nippy/thaw-from-file
     (str "input/" year "/input" day ".nippy")
     {:password [:salted password]})))

(defn save-input [year day]
  (let [password (.get dotenv "INPUT_SALT")]
    (nippy/freeze-to-file
     (str "input/" year "/input" day ".nippy")
     (slurp "input.txt")
     {:password [:salted password]})))
