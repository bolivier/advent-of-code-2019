(ns aoc.intcode
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(defn tokenize [input]
  (mapv #(Integer/parseInt %) (str/split input #",")))

(def exited? (atom false))
(def program-output (atom []))

(defonce position-mode 0)
(defonce immediate-mode 1)

(defn exit [& _]
  (reset! exited? true))

(def pc-change {1 4
                2 4
                3 2
                4 2
                5 3
                6 3
                7 4
                8 4
                99 1})

(def jump-instruction? #{5 6})

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

;; This needs to be refactored to support variadic input.
(def get-input (constantly 5))

(s/def ::intcode (s/and (s/every int?)
                        #(< 0 (count %) 4)))

(defn get-param-value-with-mode [param mode memory]
  (cond
    (= 1 mode) param
    (= 0 mode) (get memory param)
    :else (throw (Exception. (str "Given bad values to get in program: " [param mode])))))

(defmulti get-new-pc-from-jump (fn [_ {:keys [instruction]} _ _]
                                 instruction))
(defmethod get-new-pc-from-jump 5 [pc
                                   {:keys [instruction param-modes]}
                                   params
                                   program]
  (if (zero? (get-param-value-with-mode
              (first params)
              (first param-modes)
              program))
    (+ pc
       (pc-change instruction))
    (get-param-value-with-mode
     (second params)
     (second param-modes)
     program)))

(defmethod get-new-pc-from-jump 6 [pc
                                   {:keys [instruction param-modes]}
                                   params
                                   program]
  (if (zero? (get-param-value-with-mode
              (first params)
              (first param-modes)
              program))
    (get-param-value-with-mode
     (second params)
     (second param-modes)
     program)
    (+ pc
       (pc-change instruction))))

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

(defn parse-tick-instruction [computer]
  (let [{:keys [intcode/pc intcode/memory]} computer
        intcode                             (parse-intcode pc memory)
        parsed-opcode                       (parse-opcode (first intcode))
        {:keys [instruction param-modes]}   parsed-opcode]
    #:intcode{:instruction instruction
              :params      (into [] (drop 1 intcode))
              :param-modes param-modes}))

(defn output [n & _]
  (swap! program-output conj n))

(defn less-than [a b]
  (if (< a b)
    1
    0))

(defn equals [a b]
  (if (= a b)
    1
    0))

(defn no-op [& _]
  nil)

(defn get-op-fn [instruction]
  (case instruction
    99 exit
    1 +
    2 *
    3 get-input
    4 output

    5 no-op
    6 no-op

    7 less-than
    8 equals
    #_(throw (Exception. (str "Used unsupported opcode: " instruction)))))

(defn side-effect? [instruction]
  (#{4 5 6 99} instruction))

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

(defn get-updated-pc [computer]
  (let [{:keys [intcode/pc intcode/memory]} computer

        intcode               (parse-intcode pc memory)
        parsed-opcode         (parse-opcode (first intcode))
        {:keys [instruction]} parsed-opcode]
    (if (jump-instruction? instruction)
      (get-new-pc-from-jump pc
                            parsed-opcode
                            (rest intcode)
                            memory)
      (+ pc (pc-change instruction)))))

(defn get-updated-memory [computer]
  (let [{:keys [intcode/pc intcode/memory]} computer
        intcode                             (parse-intcode pc memory)
        params                              (rest intcode)
        parsed-opcode                       (parse-opcode (first intcode))
        {:keys [instruction param-modes]}   parsed-opcode
        op                                  (get-op-fn instruction)
        location                            (last params)
        val                                 (op
                                             (get-param-value-with-mode
                                              (first params)
                                              (first param-modes)
                                              memory)
                                             (get-param-value-with-mode
                                              (second params)
                                              (second param-modes)
                                              memory))]
    (if (side-effect? instruction)
      memory
      (assoc memory location val))))

(defn tick-computer [computer]
  (assoc computer
         :intcode/pc (get-updated-pc computer)
         :intcode/memory (get-updated-memory computer)))

(defn run-computer [computer]
  (last (take-while
         #(and (< (:intcode/pc %) (count (:intcode/memory %)))
               (not= 99 (current-opcode computer)))
         (iterate tick-computer computer))))
