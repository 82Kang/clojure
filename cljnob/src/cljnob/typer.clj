(ns typer
  (:require [clojure.data.json :as json]))

(def words-db ["regular" "remix" "regex" "compreg" "authority" "autograph" "regraph" "zebra"])

;; first implementation, substring based search
(defn getmatches [w words]
  (filter identity 
          (map 
           (partial re-matches (re-pattern (str ".*" w ".*")))
           words)))

(defn add-word [w file]
  (spit w file))

