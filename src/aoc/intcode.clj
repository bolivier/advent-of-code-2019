(ns aoc.intcode)

(def exited? (atom false))

(defn exit [_ _]
  (reset! exited? true))

(defn parse-intcode
  "Returns a fn that takes a program and performs the intcode on it."
  [intcode]
  (let [[opcode a b location] intcode
        op (cond
             (= 99 opcode) exit
             (= 1 opcode) +
             (= 2 opcode) *
             :else nil)]
    (fn [program]
      (let [val (op (nth program a nil) (nth program b nil))]
        (if @exited?
          program
          (assoc program location val))))))

(defn run [tokens]
  (reset! exited? false)
  (reduce (fn [program modifier] (modifier program))
          (into [] tokens)
          (map parse-intcode (partition 4 tokens))))

