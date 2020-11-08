(ns aoc.day-2
  (:require [aoc.intcode :refer [create-computer run-computer]]))

(defn solution-1 [program-string]
  (-> program-string
      create-computer
      (assoc-in [:intcode/memory 1] 12)
      (assoc-in [:intcode/memory 2] 2)
      run-computer
      :intcode/memory
      (get 0)))

(defn update-inputs [computer a b]
  (-> computer
      (assoc-in [:intcode/memory 1] a)
      (assoc-in [:intcode/memory 2] b)))

(def test-val 19690720)

(defn solution-2 [input]
  (let [check-list (for [noun (range 100)
                         verb (range 100)]
                     [noun verb])]
    (loop [active-check (first check-list)
           remaining (rest check-list)]
      (let [[noun verb] active-check
            output (-> input
                       create-computer
                       (update-inputs noun verb)
                       run-computer
                       :intcode/memory
                       first)]
        (if (= test-val output)
          [noun verb]
          #_(+ (* 100 noun)
               verb)
          (if (empty? remaining)
            nil
            (recur (first remaining)
                   (rest remaining))))))))

(comment

  (def input "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,5,19,23,2,10,23,27,1,27,5,31,2,9,31,35,1,35,5,39,2,6,39,43,1,43,5,47,2,47,10,51,2,51,6,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,1,71,5,75,1,13,75,79,1,6,79,83,2,83,13,87,1,87,6,91,1,10,91,95,1,95,9,99,2,99,13,103,1,103,6,107,2,107,6,111,1,111,2,115,1,115,13,0,99,2,0,14,0")

  (def input "1,9,10,3,2,3,11,0,99,30,40,50")

  (def test (into [] (tokenize input)))
  (def test-case [3500 9 10 70 2 3 11 0 99 30 40 50])
  (def ic [99 30 40 50]))
