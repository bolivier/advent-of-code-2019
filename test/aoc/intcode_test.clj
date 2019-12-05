(ns aoc.intcode-test
  (:require [aoc.intcode :refer [execute-intcode parse-intcode parse-opcode run change-in-pc]]
            [midje.sweet :refer :all]))

(fact "the opcode 1002 is parsed correctly"
      (parse-opcode 1002) => {:instruction 2
                              :parameter-modes [0 1 0]})

(fact "last element of a program is grabbed properly"
      (parse-intcode 4 [1201 3 4 3 99]) => [99])

(fact "output increments the pc by 2"
      (change-in-pc 4) => 2)

(fact "outputting should not change the program"
      (execute-intcode [4 3] [4 3 99]) => [4 3 99])
