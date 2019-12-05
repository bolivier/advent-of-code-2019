(ns aoc.intcode
  (:require [aoc.utils :refer [parse-int]]
            [clojure.spec.alpha :as s]))

(def exited? (atom false))

(defonce position-mode 0)
(defonce immediate-mode 1)

(defn exit [& _]
  (reset! exited? true))

(def operations {1 +
                 2 *})

(s/def ::opcode (s/and int? #(< 999 % 9999)))

(defn parse-opcode [opcode]
  (let [s (seq (format "%05d" opcode))
        [backwards-code parameter-modes] (split-at 2 (reverse s))
        instruction (->> backwards-code
                         reverse
                         (clojure.string/join "")
                         Integer/parseInt)]
    {:instruction instruction
     :parameter-modes  (mapv
                        (comp parse-int str)
                        parameter-modes)}))
(s/fdef parse-opcode
  :args (s/cat :opcode ::opcode))

(defn get-program-val [parameter mode program]
  (if (= mode immediate-mode)
    parameter
    (get program parameter)))

(def get-input (constantly 1))

(defn change-in-pc [opcode]
  (let [{:keys [instruction]} (parse-opcode opcode)]
    (cond
      (or (= 2 instruction)
          (= 1 instruction)) 4
      (or (= 3 instruction)
          (= 4 instruction)) 2
      (= 99 instruction) 1)))

(defn parse-intcode [pc program]
  (let [opcode (nth program pc)
        size (change-in-pc opcode)
        intcode (subvec program pc (+ size pc))]
    intcode))

(defn get-op-fn [instruction]
  (cond
    (= 99 instruction) exit
    (= 1 instruction) +
    (= 2 instruction) *
    (= 3 instruction) get-input
    :else identity))

(defn execute-intcode
  "Returns a fn that takes a program and performs the intcode on it."
  [intcode]
  (let [[raw-opcode & params] intcode
        {:keys [instruction
                parameter-modes]} (parse-opcode raw-opcode)
        op (get-op-fn instruction)]

    (fn [program]
      (let [val (apply op
                       (take 2 (map #(apply get-program-val %)
                                    (map vector
                                         params
                                         parameter-modes
                                         (repeat program)))))
            location (last params)]
        (if @exited?
          program
          (assoc program location val))))))


(s/def ::token int?)
(defn run [tokens]
  (reset! exited? false)
  (loop [program tokens
         pc 0]
    (if @exited?
      program
      (let [pc-inc (change-in-pc (nth program pc))
            intcode (parse-intcode pc program)]
        (recur ((execute-intcode intcode) program)
               (+ pc pc-inc))))))
(s/fdef run
  :args (s/cat :tokens (s/coll-of ::token)))

(comment
  (def tokens (into [] (aoc.day-2/tokenize "1002,4,3,4,33")))
  (def program (into [] tokens))
  (def intcode (parse-intcode 0 program))
  )
