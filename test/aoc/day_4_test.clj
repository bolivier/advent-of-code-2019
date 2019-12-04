(ns aoc.day-4-test
  (:use midje.sweet)
  (:require [aoc.day-4 :refer [only-two-adjacent-same?]]))

(def test-cases [234559 true
                 233368 false
                 111122 true])


(fact "234559 has two adjacent"
      (only-two-adjacent-same? 234559) => true)

(fact "233368 does not have two adjacent"
      (only-two-adjacent-same? 233368) => false)

(fact "111122 has two adjacent"
      (only-two-adjacent-same? 111122) => true)




