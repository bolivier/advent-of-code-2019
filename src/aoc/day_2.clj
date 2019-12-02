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

(defn run [tokens]
  (do
    (reset! exited? false)
    (reduce (fn [program modifier] (modifier program))
            (into [] tokens)
            (map parse-intcode (partition 4 tokens)))))

(defn solution-1
  ([program-string]
   (-> program-string
       tokenize
       run))
  ([]
   (solution-1 input)))

(defn generate-trial-program [program a b]
  (concat [(first program) a b] (drop 3 program)))

(def test-val 19690720)

(defn solution-2 []
  (let [tokens (tokenize real-input)
        check-list (for [noun (range 100)
                         verb (range 100)
                         :let [program (generate-trial-program tokens noun verb)]]
                     [(first (run program)) noun verb])]
    (let [[[_ noun verb]] (filter (fn [[val _ _]] (= test-val val)) check-list)]
      (+ (* 100 noun)
         verb))))

(comment

  (def real-input "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,5,19,23,2,10,23,27,1,27,5,31,2,9,31,35,1,35,5,39,2,6,39,43,1,43,5,47,2,47,10,51,2,51,6,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,1,71,5,75,1,13,75,79,1,6,79,83,2,83,13,87,1,87,6,91,1,10,91,95,1,95,9,99,2,99,13,103,1,103,6,107,2,107,6,111,1,111,2,115,1,115,13,0,99,2,0,14,0")
  
  (def input "1,9,10,3,2,3,11,0,99,30,40,50")

  (def test (into [] (tokenize input)))
  (def test-case [3500 9 10 70 2 3 11 0 99 30 40 50])
  (def ic [99 30 40 50])
  )
