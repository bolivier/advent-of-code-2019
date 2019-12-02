(ns aoc.day-2)

(defn tokenize [input]
  (map #(Integer/parseInt %) (clojure.string/split input #",")))

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

(defn solution-1
  ([program-string]
   (let [tokens (tokenize program-string)]
    (do
      (reset! exited? false)
      (reduce (fn [program modifier] (modifier program))
              (into [] tokens)
              (map parse-intcode (partition 4 tokens))))))
  ([]
   (solution-1 input)))

(comment

  (def real-input "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,5,19,23,2,10,23,27,1,27,5,31,2,9,31,35,1,35,5,39,2,6,39,43,1,43,5,47,2,47,10,51,2,51,6,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,1,71,5,75,1,13,75,79,1,6,79,83,2,83,13,87,1,87,6,91,1,10,91,95,1,95,9,99,2,99,13,103,1,103,6,107,2,107,6,111,1,111,2,115,1,115,13,0,99,2,0,14,0")
  
  (def input "1,9,10,3,2,3,11,0,99,30,40,50")

  (def test (into [] (tokenize input)))
  (def test-case [3500 9 10 70 2 3 11 0 99 30 40 50])
  (def ic [99 30 40 50])
  )
