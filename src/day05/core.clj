(ns day05.core
  (:require [taoensso.timbre :as timbre :refer [spy]]
            [intcode.core :as intcode]))

(defn solve-part-one [program]
  (let [in-buff (atom [1])
        out-buff (atom [])]
    (intcode/execute program in-buff out-buff)
    (last @out-buff)))

(assert (= 9654885 (solve-part-one (as-> (slurp "src/day05/input.txt") $
                                         (clojure.string/split $ #",")
                                         (mapv (comp #(Integer/parseInt %) clojure.string/trim) $)))))

(defn solve-part-two [program]
  (let [in-buff (atom [5])
        out-buff (atom [])]
    (intcode/execute program in-buff out-buff)
    (println @out-buff)
    (last @out-buff)))

(defn -main []
  (let [program (as-> (slurp "src/day05/input.txt") $
                      (clojure.string/split $ #",")
                      (mapv (comp #(Integer/parseInt %) clojure.string/trim) $))]
    (println "Part 1 >>> " (solve-part-one program))
    (println "Part 2 >>> " (solve-part-two program))))

(-main )
