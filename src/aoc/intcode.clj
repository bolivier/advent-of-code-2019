(ns aoc.intcode
  (:require [aoc.utils :refer [parse-int]]
            [clojure.spec.alpha :as s]
            [clojure.core.async :refer [>! <! >!! <!! go chan]]))

(defn tokenize [input]
  (mapv #(Integer/parseInt %) (clojure.string/split input #",")))

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

(def jump-instructions #{5 6})

(s/def ::opcode (s/and int? #(< 999 % 9999)))

(defn valid-instruction? [instruction]
  (contains? pc-change instruction))

(defn parse-opcode [opcode]
  (let [s (seq (format "%05d" opcode))
        [backwards-code parameter-modes] (split-at 2 (reverse s))
        instruction (->> backwards-code
                         reverse
                         (clojure.string/join "")
                         Integer/parseInt)]
    (if (not (valid-instruction? instruction))
      (throw (Exception. (str "Used an invalid instruction " instruction)))
      {:instruction instruction
       :parameter-modes  (mapv
                          (comp parse-int str)
                          parameter-modes)})))
(s/fdef parse-opcode
  :args (s/cat :opcode ::opcode))

;; This needs to be refactored to support variadic input.
(def get-input (constantly 5))

(s/def ::intcode (s/and (s/every int?)
                        #(< 0 (count %) 4)))

(defn get-param-value-with-mode [param mode program]
  (cond
    (= 1 mode) param
    (= 0 mode) (get program param)
    :else (throw (Exception. (str "Given bad values to get in program: " [param mode])))))

(defmulti get-new-pc-from-jump (fn [_ {:keys [instruction]} _ _]
                                 instruction))
(defmethod get-new-pc-from-jump 5 [pc
                                   {:keys [instruction parameter-modes]}
                                   params
                                   program]
  (if (zero? (get-param-value-with-mode
              (first params)
              (first parameter-modes)
              program))
    (+ pc
       (pc-change instruction))
    (get-param-value-with-mode
     (second params)
     (second parameter-modes)
     program)))

(defmethod get-new-pc-from-jump 6 [pc
                                   {:keys [instruction parameter-modes]}
                                   params
                                   program]
  (if (zero? (get-param-value-with-mode
              (first params)
              (first parameter-modes)
              program))
    (get-param-value-with-mode
     (second params)
     (second parameter-modes)
     program)
    (+ pc
       (pc-change instruction))))

(defn get-new-pc [intcode pc program]
  (let [parsed-intcode (parse-opcode (first intcode))
        instruction (:instruction parsed-intcode)]
    (if (jump-instructions instruction)
      (get-new-pc-from-jump
       pc
       parsed-intcode
       (rest intcode)
       program)
      (+ pc (pc-change instruction)))))

(defn parse-intcode [pc program]
  (let [opcode (nth program pc)
        instruction (:instruction (parse-opcode opcode))]
    (mapv #(get program %)
          (range pc (+ pc
                       (pc-change instruction))))))

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
  (#{4 5 6} instruction))

(defn execute-intcode
  "Returns a fn that takes a program and performs the intcode on it."
  [intcode program]
  (let [[raw-opcode & params] intcode
        {:keys [instruction
                parameter-modes]} (parse-opcode raw-opcode)
        op (get-op-fn instruction)
        location (last params)
        val (apply op
                   (take 2 (map #(apply get-param-value-with-mode %)
                                (map vector
                                     params
                                     parameter-modes
                                     (repeat program)))))]
    (if (or @exited? (side-effect? instruction))
      program
      (assoc program location val))))

(s/def ::token int?)
(s/def ::program-input (s/coll-of ::token))
(s/def ::program-output (s/coll-of ::token))
(defn run
  ([tokens] (run tokens []))
  ([tokens inputs]
   (reset! exited? false)
   (reset! program-output [])
   (loop [program tokens
          pc 0]
     (if (or @exited?
             (< (count program) pc))
       program
       (let [intcode (parse-intcode pc program)
             new-pc (get-new-pc intcode pc program)]
         (recur (execute-intcode intcode program)
                new-pc)))))
  @program-output)

(s/fdef run
  :args (s/cat :tokens (s/coll-of ::token)))

(comment
  (def tokens (into [] (aoc.day-2/tokenize "1002,4,3,4,33")))


  (run (tokenize "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"))

  (def tokens (tokenize (slurp "resources/day_5.input")))

  (run tokens)

  ;; channel
  ;; that has some buffered stuff on it
  ;; that will query a user when that stuff runs out

  (defn std-in [data]
    (let [c (chan)]
      (do
        (go (doseq [x data]
              (>!! c x)))
        c)))
  )
