(ns day09.core
  (:require [taoensso.timbre :as timbre :refer [spy]]
            [intcode.core :as intcode]))

(defn solve-part-one [program]

  )

(defn -main []
  (let [program (intcode/parse-intcode-program-file "src/day09/input.txt")]
    (println "Part 1 >>> " (solve-part-one program))
    ))
