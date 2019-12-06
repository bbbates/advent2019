(ns day02.core
  (:require [taoensso.timbre :as timbre :refer [spy]]))

(def ops
  {1  +
   2  *
   99 :halt})

(defn execute
  "Execute the program, returning the new memory map after execution has completed"
  [program]
  (loop [memory-map program
         op-ptr 0]
    (if (>= op-ptr (count memory-map))
      memory-map
      (let [instr (ops (get memory-map op-ptr))]
        (if (= instr :halt)
          memory-map
          (let [[_ in1 in2 output] (subvec memory-map op-ptr (+ op-ptr 4))]
            (recur (assoc memory-map output (instr (get memory-map in1) (get memory-map in2)))
                   (+ op-ptr 4))))))))

(assert (= (execute [1, 0, 0, 0, 99]) [2 0 0 0 99]))
(assert (= (execute [2, 3, 0, 3, 99]) [2 3 0 6 99]))
(assert (= (execute [2, 4, 4, 5, 99, 0]) [2,4,4,5,99,9801]))
(assert (= (execute [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))

(defn solve-part-one [program]
  (first (execute (-> program (assoc 1 12) (assoc 2 2)))))

(defn -main []
  (let [program (as-> (slurp "src/day02/input.txt") $
                      (clojure.string/split $ #",")
                      (mapv #(Integer/parseInt %) $))]
    (println "Part 1 >>> " (solve-part-one program))))

(-main)
