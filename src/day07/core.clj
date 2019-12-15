(ns day07.core
  (:require [taoensso.timbre :as timbre :refer [spy]]
            [intcode.core :as intcode]))

(defn- execute-amp
  [program phase in]
  (let [in (atom [phase in])
        out (atom [])]
    (intcode/execute program in out)
    (last @out)))

(defn- execute-all-amps
  [program [a b c d e]]
  (->>
    (execute-amp program a 0)
    (execute-amp program b)
    (execute-amp program c)
    (execute-amp program d)
    (execute-amp program e)))

(defn solve-part-one [program]
  (let [phases (range 0 5)
        phase-combs (set (for [a phases b phases c phases d phases e phases
                                     :when (distinct? a b c d e)]
                                 [a b c d e]))]
    (->> phase-combs
         (map (partial execute-all-amps program))
         (sort >)
         (first))))

(assert (= (solve-part-one [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) 43210))
(assert (= (solve-part-one [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                            101,5,23,23,1,24,23,23,4,23,99,0,0]) 54321))
(assert (= (solve-part-one [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                            1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) 65210))


(defn -main []
  (let [program (as-> (slurp "src/day07/input.txt") $
                      (clojure.string/split $ #",")
                      (mapv (comp #(Integer/parseInt %) clojure.string/trim) $))]
    (println "Part 1 >>> " (solve-part-one program))
    #_(println "Part 2 >>> " (solve-part-two program))))
