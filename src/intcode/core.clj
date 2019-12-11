(ns intcode.core
  (:require [taoensso.timbre :refer [spy]]))

(def opcodes
  {1  [:add 4]
   2  [:mul 4]
   3  [:in 2]
   4  [:out 2]
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
(defmulti execute-op-code (fn [_ [instr] _] instr))

(defmethod execute-op-code :halt
  [memory-map _ _]
  [memory-map :halt])

(defn- param-value
  [memory-map addr param-mode]
  (if (= :imm param-mode)
    addr
    (get memory-map addr)))

(defn- execute-arithmetic-op-code
  [op memory-map [_ param1-mode param2-mode] {:keys [addr]}]
  (let [next-addr (+ addr 4)
        [_ in1 in2 output] (subvec memory-map addr next-addr)]
    [(assoc memory-map output
                       (op (param-value memory-map in1 param1-mode)
                           (param-value memory-map in2 param2-mode)))
     next-addr]))

(defmethod execute-op-code :add
  [memory-map parsed-op-code state]
  (execute-arithmetic-op-code + memory-map parsed-op-code state))

(defmethod execute-op-code :mul
  [memory-map parsed-op-code state]
  (execute-arithmetic-op-code * memory-map parsed-op-code state))

(defmethod execute-op-code :in
  [memory-map _ {:keys [addr in-buff]}]
  (let [next-addr (+ addr 2)
        [_ in-addr] (subvec memory-map addr next-addr)
        input (first @in-buff)]
    (swap! in-buff (comp vec rest))
    [(assoc memory-map in-addr input)
     next-addr]))

(defmethod execute-op-code :out
  [memory-map _ {:keys [addr out-buff]}]
  (let [next-addr (+ addr 2)
        [_ out-addr] (subvec memory-map addr next-addr)]
    (swap! out-buff (comp vec conj) (get memory-map out-addr))
    [memory-map next-addr]))

(defn execute
  "Execute the program, returning the new memory map after execution has completed"
  [program & [in-buff out-buff]]
  (let [base-state {:in-buff in-buff :out-buff out-buff}]
    (loop [memory-map program
           op-ptr 0]
      (if (>= op-ptr (count memory-map))
        memory-map
        (let [parsed-op-code (parse-op-code (get memory-map op-ptr))
              [new-memory-map next-op-ptr]
              (execute-op-code memory-map parsed-op-code (assoc base-state :addr op-ptr))]
          (if (= next-op-ptr :halt)
            new-memory-map
            (recur new-memory-map next-op-ptr)))))))

;; Day 2 assertions
(assert (= (execute [1, 0, 0, 0, 99]) [2 0 0 0 99]))
(assert (= (execute [2, 3, 0, 3, 99]) [2 3 0 6 99]))
(assert (= (execute [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]))
(assert (= (execute [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]))

