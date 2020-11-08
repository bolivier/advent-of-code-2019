(ns aoc.intcode
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(defn tokenize [input]
  (mapv #(Integer/parseInt %) (str/split input #",")))

(def pc-change {:arithmetic 4
                1 4
                2 4
                3 2
                4 2
                5 3
                6 3
                7 4
                8 4
                99 1})

(s/def ::opcode (s/and int? #(< 999 % 9999)))

(defn valid-instruction? [instruction]
  (contains? pc-change instruction))

(defn parse-opcode
  "opcodes are 5 digit integers that represent the parameter modes plus the operation itself.

  They are 'little endian' for the operation - it's the 10's place"
  [opcode]
  (let [instruction (mod opcode 100)
        parameter-mode-code (quot opcode 100)]
    (if (not (valid-instruction? instruction))
      (throw (Exception. (str "Used an invalid instruction " instruction)))
      {:instruction instruction
       :param-modes (->> parameter-mode-code
                         (iterate #(int (/ % 10)))
                         (take 3)
                         (mapv #(mod % 10)))})))
(s/fdef parse-opcode
  :args (s/cat :opcode ::opcode))

(s/def ::intcode (s/and (s/every int?)
                        #(< 0 (count %) 4)))

(defn get-param-value-with-mode [param mode memory]
  (cond
    (= 1 mode) param
    (= 0 mode) (get memory param)
    :else (throw (Exception. (str "Given bad values to get in program: " [param mode])))))

(s/def :intcode/instruction #{1 2 3 4 5 6 7 8 99})
(s/def :intcode/params (s/and
                        #(= 3 (count %))
                        (s/coll-of int?)))
(s/def :intcode/param-modes (s/and
                             #(= 3 (count %))
                             (s/coll-of #{0 1})))
(s/def :intcode/tick-instruction (s/keys :req [:intcode/instruction :intcode/params :intcode/param-modes]))

(defn parse-intcode
  "Grab multi digit 'intcode'"
  [pc program]
  (let [opcode (nth program pc)
        instruction (:instruction (parse-opcode opcode))]
    (mapv #(get program %)
          (range pc (+ pc
                       (pc-change instruction))))))

(defn parse-tick-instruction
  "Take an incode computer and extract info about the next instruction
  to do."
  [computer]
  (let [{:keys [intcode/pc intcode/memory]} computer
        intcode                             (parse-intcode pc memory)
        parsed-opcode                       (parse-opcode (first intcode))
        {:keys [instruction param-modes]}   parsed-opcode]
    #:intcode{:instruction instruction
              :params      (into [] (drop 1 intcode))
              :param-modes param-modes}))

(s/def :incode/output (s/coll-of int?))
(s/def :incode/input (s/coll-of int?))
(s/def :intcode/pc int?)
(s/def :intcode/memory (s/coll-of int?))
(s/def :intcode/computer (s/keys :req [:intcode/memory :intcode/pc :intcode/input :intcode/output]))

(s/fdef parse-tick-instruction
  :args (s/cat :computer :intcode/computer)
  :ret :intcode/tick-instruction)

(s/def ::token int?)

(defn create-computer
  ([memory] (create-computer memory []))
  ([memory input]
   {:intcode/memory (tokenize memory)
    :intcode/input input
    :intcode/output []
    :intcode/pc 0}))

(defn current-opcode [{:keys [intcode/memory intcode/pc]}]
  (mod (get memory pc) 100))

(defn execute-arithmetic-instruction [computer fn]
  (let [instruction (parse-tick-instruction computer)
        memory (:intcode/memory computer)
        [param1 param2 memory-location] (:intcode/params instruction)
        [mode1 mode2] (:intcode/param-modes instruction)

        arg1 (get-param-value-with-mode param1 mode1 memory)
        arg2 (get-param-value-with-mode param2 mode2 memory)

        value (fn arg1 arg2)]
    (-> computer
        (assoc-in [:intcode/memory memory-location] value)
        (update :intcode/pc (partial + (pc-change :arithmetic))))))

(defmulti execute-instruction
  (fn [computer]
    (:intcode/instruction (parse-tick-instruction computer))))

(defmethod execute-instruction 99 [computer]
  (update computer :intcode/pc inc))

;; It might be helpful to extract some data constants from these methods to help
;; define the instructions in data instead of in code.
(defmethod execute-instruction 1 [computer]
  (execute-arithmetic-instruction computer +))

(defmethod execute-instruction 2 [computer]
  (execute-arithmetic-instruction computer *))

(defmethod execute-instruction 3 [computer]
  (let [instruction (parse-tick-instruction computer)
        input-val (first (:intcode/input computer))
        memory-location (first (:intcode/params instruction))]
    (-> computer
        (assoc-in [:intcode/memory memory-location] input-val)
        (update :intcode/input #(into [] (rest %)))
        (update :intcode/pc (partial + (pc-change 3))))))

(defmethod execute-instruction 4 [computer]
  (let [instruction (parse-tick-instruction computer)
        memory-location (first (:intcode/params instruction))]
    (-> computer
        (update :intcode/output #(conj % (get-in computer [:intcode/memory memory-location])))
        (update :intcode/pc (partial + (pc-change 4))))))

(defn tick-computer [computer]
  (execute-instruction computer))

(defn non-exit-instruction [computer]
  (not= 99 (current-opcode computer)))

(defn has-more-memory [computer]
  (< (:intcode/pc computer) (count (:intcode/memory computer))))

(defn run-computer [computer]
  (first
   (drop-while
    (every-pred non-exit-instruction has-more-memory)
    (iterate tick-computer computer))))
