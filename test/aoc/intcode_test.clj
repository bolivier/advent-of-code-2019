(ns aoc.intcode-test
  (:use [midje.sweet])
  (:require [aoc.intcode :refer [parse-opcode]]))

(fact "the opcode 1002 is parsed correctly"
      (parse-opcode 1002) => {:instruction 2
                              :parameter-modes [0 1 0]})
