(ns aoc.day-5
  (:require [aoc.intcode :refer [run tokenize]]))

(def input (into [] (tokenize (slurp "resources/day_5.input"))))

(defn solution-1 []
  (run input))
