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


(facts "param fetching w/ mode"
       (fact "fetching with immediate mode should return the param"
             (get-param-value-with-mode 2 1 [3 4 5 6]) => 2))

(facts "Opcode 7"
       (fact "Opcode 7 stores 1 in 5 when (< second first"
             (execute-intcode [7 1 2 3] [7 1 2 3]) => [7 1 2 1]))

(facts "Opcode 8"
       (fact "Opcode 8 stores 1 in 5 when (= second first)"
             (execute-intcode [8 1 1 3] [5 1 1 3]) => [5 1 1 1]))

(fact "This program outputs 999 for inputs < 8, 1000 if == 8, 1001 otherwise"
      (run (tokenize "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")) => [999])
