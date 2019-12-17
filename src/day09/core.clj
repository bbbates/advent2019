(ns day09.core
  (:require [taoensso.timbre :as timbre :refer [spy]]
            [intcode.core :as intcode]))

(defn solve-part-one [program]
  (let [in (atom [1])
        out (atom [])]
    (intcode/execute program in out)
    @out))

(defn solve-part-two [program]
  (let [in (atom [2])
        out (atom [])]
    (intcode/execute program in out)
    @out))

(defn -main []
  (let [program (intcode/parse-intcode-program-file "src/day09/input.txt")]
    (println "Part 1 >>> " (solve-part-one program))
    (println "Part 2 >>> " (solve-part-two program))))
