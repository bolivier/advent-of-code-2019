(ns aoc.intcode-test
  (:require [aoc.intcode :refer :all]
            [midje.sweet :refer :all]))

(fact "the opcode 1002 is parsed correctly"
      (parse-opcode 1002) => {:instruction 2
                              :parameter-modes [0 1 0]})

(fact "last element of a program is grabbed properly"
      (parse-intcode 4 [1201 3 4 3 99]) => [99])

(fact "outputting should not change the program"
      (execute-intcode [4 3] [4 3 99]) => [4 3 99])

(every?
 identity
 (for [[intcode pc] [[[1201 2 3 3] 4]
                     [[5 1 5] 0]
                     [[5 0 5] 5]
                     [[6 1 5] 5]
                     [[6 0 5] 0]]]
   (fact (str "intcode (" intcode ") increments the pc by " pc)
         (get-new-pc intcode 0) => pc)))

(facts "Opcode 7"
       (fact "Opcode 7 stores 1 in 5 when (< second first"
             (execute-intcode [7 1 2 3] [7 1 2 3]) => [7 1 2 1]))

(facts "Opcode 8"
       (fact "Opcode 8 stores 1 in 5 when (= second first)"
             (execute-intcode [8 1 1 3] [5 1 1 3]) => [5 1 1 1]))
