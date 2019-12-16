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

(assert (= (solve-part-one [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]) 43210))
(assert (= (solve-part-one [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
                            101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]) 54321))
(assert (= (solve-part-one [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
                            1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]) 65210))

(defn- execute-amp-with-state
  [state in]
  (let [in (atom in)
        out (atom [])]
    [(intcode/execute-with-state state in out) (last @out)]))

(defn- feedback-loop
  [program phases]
  (let [amp-cycle (cycle phases)]
    (reduce (fn [{:keys [out cycles programs]} phase]
              (let [program (get programs phase)
                    in (if (< cycles (count phases)) [phase out] [out])
                    [program-state new-out] (execute-amp-with-state program in)]
                (if (and (= :halt (:state program-state)) (= phase (last phases)))
                  (reduced new-out)
                  {:out new-out
                   :cycles (inc cycles)
                   :programs (assoc programs phase program-state)})))
            {:out 0
             :cycles 0
             :programs (apply array-map (interleave phases (repeat {:program program :op-ptr 0})))}
            amp-cycle)))


(defn solve-part-two [program]
  (let [phases (range 5 10)
        phase-combs (set (for [a phases b phases c phases d phases e phases
                               :when (distinct? a b c d e)]
                           [a b c d e]))]
    (->> phase-combs
         (map (partial feedback-loop program))
         (sort >)
         (first))))

(assert (= (solve-part-two [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                            27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]) 139629729))
(assert (= (solve-part-two [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                            -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                            53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]) 18216))


(defn -main []
  (let [program (as-> (slurp "src/day07/input.txt") $
                      (clojure.string/split $ #",")
                      (mapv (comp #(Integer/parseInt %) clojure.string/trim) $))]
    (println "Part 1 >>> " (solve-part-one program))
    (println "Part 2 >>> " (solve-part-two program))))
