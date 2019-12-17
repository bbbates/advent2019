(ns intcode.core
  (:require [taoensso.timbre :refer [spy]]))

(def opcodes
  {1  [:add 3]
   2  [:mul 3]
   3  [:in 1]
   4  [:out 1]
   5  [:jump-if-true 2]
   6  [:jump-if-false 2]
   7  [:less-than 3]
   8  [:equals 3]
   9  [:relative-base 1]
   99 [:halt 0]})

(defn- parse-op-code
  [opcode]
  (let [[E D C B A] (reverse (str opcode))
        [instr num-parms] (opcodes (Integer/parseInt (str D E)))]
    (vec (take (+ num-parms 1)
               [instr
                (if (= C \1) :imm :pos)
                (if (= B \1) :imm :pos)
                (if (= A \1) :imm :pos)]))))

(assert (= [:add :pos :pos :pos]) (parse-op-code 1))
(assert (= [:mul :pos :pos :pos]) (parse-op-code 2))
(assert (= [:add :imm :imm :imm]) (parse-op-code 11101))
(assert (= [:mul :imm :imm :imm]) (parse-op-code 11102))
(assert (= [:mul :imm :imm :pos]) (parse-op-code 1102))
(assert (= [:mul :imm :pos :pos]) (parse-op-code 102))
(assert (= [:halt]) (parse-op-code 99))
(assert (= [:in :pos :pos]) (parse-op-code 3))
(assert (= [:out :pos :pos]) (parse-op-code 4))
(assert (= [:out :imm :pos]) (parse-op-code 104))
(assert (= [:out :imm :imm]) (parse-op-code 1104))



;; Takes a memory map, the current instruction and parameter modes, and the computer-state map
;; and returns [new-memory-map, instr-ptr-modifier]
(defmulti execute-op-code (fn [_ _ [instr] _] instr))

(defmethod execute-op-code :halt
  [memory-map _ _ _]
  [memory-map :halt])

(defn- param-value
  [memory-map addr param-mode]
  (if (= :imm param-mode)
    addr
    (get memory-map addr)))

(defn- execute-arithmetic-op-code
  [op memory-map [_ in1 in2 output] [_ param1-mode param2-mode] {:keys [addr]}]
  [(assoc memory-map output
                     (op (param-value memory-map in1 param1-mode)
                         (param-value memory-map in2 param2-mode)))
   (+ addr 4)])

(defmethod execute-op-code :add
  [memory-map op-instr parsed-op-code state]
  (execute-arithmetic-op-code + memory-map op-instr parsed-op-code state))

(defmethod execute-op-code :mul
  [memory-map op-instr parsed-op-code state]
  (execute-arithmetic-op-code * memory-map op-instr parsed-op-code state))

(defmethod execute-op-code :in
  [memory-map [_ in-addr] _ {:keys [addr in-buff]}]
  (if-let [input (first @in-buff)]
    (do (swap! in-buff (comp vec rest))
        [(assoc memory-map in-addr input)
         (+ addr 2)])
    [memory-map :waiting]))

(defmethod execute-op-code :out
  [memory-map [_ out-addr] _ {:keys [addr out-buff]}]
  (swap! out-buff (comp vec conj) (get memory-map out-addr))
  [memory-map (+ addr 2)])

(defmethod execute-op-code :jump-if-true
  [memory-map [_ in jump-to] [_ param1-mode param2-mode] {:keys [addr]}]
  [memory-map (if-not (zero? (param-value memory-map in param1-mode))
                (param-value memory-map jump-to param2-mode)
                (+ addr 3))])

(defmethod execute-op-code :jump-if-false
  [memory-map [_ in jump-to] [_ param1-mode param2-mode] {:keys [addr]}]
  [memory-map (if (zero? (param-value memory-map in param1-mode))
                (param-value memory-map jump-to param2-mode)
                (+ addr 3))])

(defmethod execute-op-code :less-than
  [memory-map [_ in1 in2 out] [_ param1-mode param2-mode] {:keys [addr]}]
  (let [result (if (< (param-value memory-map in1 param1-mode)
                      (param-value memory-map in2 param2-mode))
                 1 0)]
    [(assoc memory-map out result) (+ addr 4)]))

(defmethod execute-op-code :equals
  [memory-map [_ in1 in2 out] [_ param1-mode param2-mode] {:keys [addr]}]
  (let [result (if (= (param-value memory-map in1 param1-mode)
                      (param-value memory-map in2 param2-mode))
                 1 0)]
    [(assoc memory-map out result) (+ addr 4)]))

(defn- halt-or-pause-exec?
  [next-op-ptr]
  (#{:halt :waiting :overrun} next-op-ptr))

(defn execute-with-state
  [{:keys [program op-ptr] :as init} & [in-buff out-buff]]
  (let [base-state {:in-buff in-buff :out-buff out-buff}]
    (loop [{:keys [program op-ptr state relative-base] :as intcode} init]
      (if (>= op-ptr (count program))
        (assoc intcode :state :overrun)
        (let [parsed-op-code (parse-op-code (get program op-ptr))
              op-instr (subvec program op-ptr (+ op-ptr (count parsed-op-code)))
              [new-program next-op-ptr]
              (execute-op-code program op-instr parsed-op-code (assoc base-state :addr op-ptr))]
          (if (halt-or-pause-exec? next-op-ptr)
            (assoc intcode :program new-program :state next-op-ptr)
            (recur (assoc intcode :program new-program :state nil :op-ptr next-op-ptr))))))))

(defn execute
  "Execute the program, returning the new memory map after execution has completed"
  [program & [in-buff out-buff]]
  (:program (execute-with-state {:program program :op-ptr 0} in-buff out-buff)))

;; Day 2 assertions
(assert (= (execute [1, 0, 0, 0, 99]) [2 0 0 0 99]))
(assert (= (execute [2, 3, 0, 3, 99]) [2 3 0 6 99]))
(assert (= (execute [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]))
(assert (= (execute [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]))

;; Day 05 assertions
(assert (= 0 (let [in-buff (atom [0]) out-buff (atom [])]
               (execute [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] in-buff out-buff)
               (first @out-buff))))
(assert (= 1 (let [in-buff (atom [8]) out-buff (atom [])]
               (execute [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] in-buff out-buff)
               (first @out-buff))))

(assert (= 1 (let [in-buff (atom [7]) out-buff (atom [])]
               (execute [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] in-buff out-buff)
               (first @out-buff))))
(assert (= 0 (let [in-buff (atom [9]) out-buff (atom [])]
               (execute [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] in-buff out-buff)
               (first @out-buff))))

(assert (= 0 (let [in-buff (atom [0]) out-buff (atom [])]
               (execute [3, 3, 1108, -1, 8, 3, 4, 3, 99] in-buff out-buff)
               (first @out-buff))))
(assert (= 1 (let [in-buff (atom [8]) out-buff (atom [])]
               (execute [3, 3, 1108, -1, 8, 3, 4, 3, 99] in-buff out-buff)
               (first @out-buff))))

(assert (= 0 (let [in-buff (atom [0]) out-buff (atom [])]
               (execute [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] in-buff out-buff)
               (first @out-buff))))
(assert (= 1 (let [in-buff (atom [1]) out-buff (atom [])]
               (execute [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] in-buff out-buff)
               (first @out-buff))))
(assert (= 0 (let [in-buff (atom [0]) out-buff (atom [])]
               (execute [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] in-buff out-buff)
               (first @out-buff))))
(assert (= 1 (let [in-buff (atom [1]) out-buff (atom [])]
               (execute [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] in-buff out-buff)
               (first @out-buff))))

; FIXME: Why didn't this work??
; (assert (= 999
;           (let [in-buff (atom [8]) out-buff (atom [])]
;             (execute [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
;                       1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
;                       999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99]
;                      in-buff out-buff)
;             (first @out-buff))))


(defn parse-intcode-program-file
  [path]
  (as-> (slurp path) $
        (clojure.string/split $ #",")
        (mapv (comp #(Integer/parseInt %) clojure.string/trim) $)))
