(ns aoc.day-6-test  
  (:require [aoc.day-6 :refer :all]
            [midje.sweet :refer :all]))

(fact "planets are parsed from strings with a `)`"
      (parse-orbit-string "B)E") => [:E :B])

