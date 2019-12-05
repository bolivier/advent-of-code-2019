(ns aoc.intcode
  (:require [aoc.utils :refer [parse-int]]
            [clojure.spec.alpha :as s]))

(def exited? (atom false))

(defonce position-mode 0)
(defonce immediate-mode 1)

(defn exit [_ _]
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

(defn parse-intcode
  "Returns a fn that takes a program and performs the intcode on it."
  [intcode]
  (let [[raw-opcode & params] intcode
        {:keys [instruction
                parameter-modes]} (parse-opcode raw-opcode)
        op (cond
             (= 99 instruction) exit
             (= 1 instruction) +
             (= 2 instruction) *
             :else nil)]

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
  (reduce (fn [program modifier] (modifier program))
          (into [] tokens)
          (map parse-intcode (partition 4 tokens))))
(s/fdef run
  :args (s/cat :tokens (s/coll-of ::token)))
