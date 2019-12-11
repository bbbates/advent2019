(ns day05.core)

(defn solve-part-one [program]
  (let [in-buff (atom [1])
        out-buff (atom [])]

    )
  )


(defn -main []
  (let [program (as-> (slurp "src/day05/input.txt") $
                      (clojure.string/split $ #",")
                      (mapv (comp #(Integer/parseInt %) clojure.string/trim) $))]
    (println "Part 1 >>> " (solve-part-one program))))
