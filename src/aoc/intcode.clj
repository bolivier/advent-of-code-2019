(ns aoc.intcode
  (:require [aoc.utils :refer [parse-int]]
            [clojure.core.async :as async :refer [<! >! >!! <!!]]
            [clojure.spec.alpha :as s]))

(defn tokenize [input]
  (map #(Integer/parseInt %) (clojure.string/split input #",")))

(def exited? (atom false))
(def program-output (atom []))

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

(defmulti get-new-pc (fn [intcode _]
                       (:instruction (parse-opcode (first intcode)))))
(defmethod get-new-pc 5 [[_ & params] pc]
  (if (zero? (first params))
    (second params)
    pc))
(defmethod get-new-pc 6 [[_ & params] pc]
  (if (zero? (first params))
    pc
    (second params)))
(defmethod get-new-pc 1 [_ pc] (+ pc 4))
(defmethod get-new-pc 2 [_ pc] (+ pc 4))
(defmethod get-new-pc 3 [_ pc] (+ pc 2))
(defmethod get-new-pc 4 [_ pc] (+ pc 2))
(defmethod get-new-pc 99 [_ _] (+ pc 1))

(defn parse-intcode [pc program]
  (let [opcode (nth program pc)]
    (mapv #(get program %)
          (range pc (get-new-pc [opcode] pc)))))

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
    (throw (Exception. (str "Used unsupported opcode: " instruction)))))

(defn side-effect? [instruction]
  (#{3 4 5 6} instruction))

(defn execute-intcode
  "Returns a fn that takes a program and performs the intcode on it."
  [intcode program]
  (let [[raw-opcode & params] intcode
        {:keys [instruction
                parameter-modes]} (parse-opcode raw-opcode)
        op (get-op-fn instruction)
        location (last params)
        val (apply op
                   (take 2 (map #(apply get-program-val %)
                                (map vector
                                     params
                                     parameter-modes
                                     (repeat program)))))]
    (if (or @exited? (side-effect? instruction))
      program
      (assoc program location val))))

(s/def ::token int?)
(defn run [tokens]
  (reset! exited? false)
  (reset! program-output [])
  (loop [program tokens
         pc 0]
    (if @exited?
      program
      (let [intcode (parse-intcode pc program)
            new-pc (get-new-pc intcode pc)]
        (recur (execute-intcode intcode program)
               new-pc))))
  (last @program-output))

(s/fdef run
  :args (s/cat :tokens (s/coll-of ::token)))

(comment
  (def tokens (into [] (aoc.day-2/tokenize "1002,4,3,4,33")))
  )
