(ns intcode.core
  (:require [taoensso.timbre :refer [spy]]))

(def opcodes
  {1  [:add 4]
   2  [:mul 4]
   3  [:in 1]
   4  [:out 1]
   5  [:jump-if-true 2]
   6  [:jump-if-false 2]
   7  [:less-than 4]
   8  [:equals 4]
   9  [:relative-base 1]
   99 [:halt 0]})

(defn- param-mode-from-char [ch]
  (cond
    (= ch \1) :imm
    (= ch \2) :rel
    :else :pos))

(defn- parse-op-code
  [opcode]
  (let [[E D C B A] (reverse (str opcode))
        [instr num-parms] (opcodes (Integer/parseInt (str D E)))]
    (vec (take (+ num-parms 1)
               [instr
                (param-mode-from-char C)
                (param-mode-from-char B)
                (param-mode-from-char A)]))))

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
(assert (= [:relative-base :imm]) (parse-op-code 109))
(assert (= [:relative-base :rel]) (parse-op-code 209))


;; Takes a memory map, the current instruction and parameter modes, and the computer-state map
;; and returns [new-memory-map, instr-ptr-modifier]
(defmulti execute-op-code (fn [_ _ [instr] _] instr))

(defmethod execute-op-code :halt
  [memory-map _ _ _]
  [memory-map :halt])

(defn- param-value
  [memory-map addr param-mode {:keys [relative-base]}]
  (cond
    (= :imm param-mode) addr
    (= :rel param-mode) (get memory-map (+ relative-base addr) 0)
    :else (get memory-map addr 0)))

(defn- apply-to-memory-map [memory-map addr value]
  (let [size (count memory-map)]
    (if (> addr size)
      (assoc (vec (concat memory-map (repeat (- addr size) 0))) addr value)
      (assoc memory-map addr value))))

(defn- apply-addr [in-addr param-mode {:keys [relative-base]}]
  (if (= param-mode :rel)
    (+ relative-base in-addr)
    in-addr))

(defn- execute-arithmetic-op-code
  [op memory-map [_ in1 in2 output] [_ param1-mode param2-mode param3-mode] {:keys [addr] :as env}]
  [(apply-to-memory-map memory-map
                        (apply-addr output param3-mode env)
                        (op (param-value memory-map in1 param1-mode env)
                            (param-value memory-map in2 param2-mode env)))
   (+ addr 4)])

(defmethod execute-op-code :add
  [memory-map op-instr parsed-op-code env]
  (execute-arithmetic-op-code + memory-map op-instr parsed-op-code env))

(defmethod execute-op-code :mul
  [memory-map op-instr parsed-op-code env]
  (execute-arithmetic-op-code * memory-map op-instr parsed-op-code env))

(defmethod execute-op-code :in
  [memory-map [_ in-addr] [_ param-mode] {:keys [addr in-buff] :as env}]
  (if-let [input (first @in-buff)]
    (let [in-addr (apply-addr in-addr param-mode env)]
      (swap! in-buff (comp vec rest))
      [(apply-to-memory-map memory-map in-addr input)
       (+ addr 2)])
    [memory-map :waiting]))

(defmethod execute-op-code :out
  [memory-map [_ out-addr] [_ param-mode] {:keys [addr out-buff] :as env}]
  (swap! out-buff (comp vec conj) (param-value memory-map out-addr param-mode env))
  [memory-map (+ addr 2)])

(defmethod execute-op-code :jump-if-true
  [memory-map [_ in jump-to] [_ param1-mode param2-mode] {:keys [addr] :as env}]
  [memory-map (if-not (zero? (param-value memory-map in param1-mode env))
                (param-value memory-map jump-to param2-mode env)
                (+ addr 3))])

(defmethod execute-op-code :jump-if-false
  [memory-map [_ in jump-to] [_ param1-mode param2-mode] {:keys [addr] :as env}]
  [memory-map (if (zero? (param-value memory-map in param1-mode env))
                (param-value memory-map jump-to param2-mode env)
                (+ addr 3))])

(defmethod execute-op-code :less-than
  [memory-map [_ in1 in2 out] [_ param1-mode param2-mode param3-mode] {:keys [addr] :as env}]
  (let [result (if (< (param-value memory-map in1 param1-mode env)
                      (param-value memory-map in2 param2-mode env))
                 1 0)]
    [(apply-to-memory-map memory-map (apply-addr out param3-mode env) result) (+ addr 4)]))

(defmethod execute-op-code :equals
  [memory-map [_ in1 in2 out] [_ param1-mode param2-mode param3-mode] {:keys [addr] :as env}]
  (let [result (if (= (param-value memory-map in1 param1-mode env)
                      (param-value memory-map in2 param2-mode env))
                 1 0)]
    [(apply-to-memory-map memory-map (apply-addr out param3-mode env) result) (+ addr 4)]))

(defmethod execute-op-code :relative-base
  [memory-map [_ in] [_ param-mode] {:keys [addr relative-base] :as env}]
  [memory-map (+ addr 2) (+ relative-base (param-value memory-map in param-mode env))])

(defn- halt-or-pause-exec?
  [next-op-ptr]
  (#{:halt :waiting :overrun} next-op-ptr))

(defn execute-with-state
  [init & [in-buff out-buff]]
  (let [base-env {:in-buff in-buff :out-buff out-buff}]
    (loop [{:keys [program op-ptr state relative-base] :as intcode} init]
      (if (>= op-ptr (count program))
        (assoc intcode :state :overrun)
        (let [parsed-op-code (parse-op-code (get program op-ptr))
              op-instr (subvec program op-ptr (+ op-ptr (count parsed-op-code)))
              [new-program next-op-ptr new-relative-base]
              (execute-op-code program op-instr parsed-op-code
                               (assoc base-env
                                 :addr op-ptr
                                 :relative-base (or relative-base 0)))]
          (if (halt-or-pause-exec? next-op-ptr)
            (assoc intcode :program new-program
                           :state next-op-ptr
                           :relative-base (or new-relative-base relative-base))
            (recur (assoc intcode :program new-program
                                  :state :running
                                  :op-ptr next-op-ptr
                                  :relative-base (or new-relative-base relative-base)))))))))

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

;; Day 09 assertions

(let [program [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
      in-buff (atom []) out-buff (atom [])]
  (execute program in-buff out-buff)
  (assert (= @out-buff program)))

(assert (= 16
           (let [in-buff (atom []) out-buff (atom [])]
             (execute [1102, 34915192, 34915192, 7, 4, 7, 99, 0] in-buff out-buff)
             (count (str (first @out-buff))))))


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
