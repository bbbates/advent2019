(ns day02.core
  (:require [taoensso.timbre :as timbre :refer [spy]]
            [intcode.core :as intcode]))

(defn- program-with-noun-and-verb
  [program noun verb]
  (-> program (assoc 1 noun) (assoc 2 verb)))

(defn solve-part-one [program]
  (first (intcode/execute (program-with-noun-and-verb program 12 2))))

(defn solve-part-two [program]
  (let [search-for 19690720
        cs (for [n (range 0 100) v (range 0 100)] [n v])]
    (let [[n v] (first
                  (filter #(= search-for (first (intcode/execute (apply program-with-noun-and-verb program %))))
                               cs))]
      (+ (* 100 n) v))))

(defn -main []
  (let [program (as-> (slurp "src/day02/input.txt") $
                      (clojure.string/split $ #",")
                      (mapv (comp #(Integer/parseInt %) clojure.string/trim) $))]
    (println "Part 1 >>> " (solve-part-one program))
    (println "Part 2 >>> " (solve-part-two program))))
