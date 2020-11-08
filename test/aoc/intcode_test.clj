(ns aoc.intcode-test
  (:require [aoc.intcode :as sut]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest parsing-opcodes
  (testing "opcode 2"
    (is (= {:instruction 2
            :param-modes [0 1 0]}
           (sut/parse-opcode 1002)))

    (is (= {:instruction 99
            :param-modes [0 1 0]}
           (sut/parse-opcode 1099)))))

(deftest parsing-intcode
  (testing "grabbing the last program element"
    (is (= (sut/parse-intcode 4 [1201 3 4 3 99])
           [99]))))

(deftest immediate-mode-testing
  (testing "param fetching with immediate mode"
    (is (= 2
           (sut/get-param-value-with-mode 2 1 [3 4 5 6])))))

(deftest creating-computer
  (is (= {:intcode/memory [1 9 10 3 2 3 11 0 99 30 40 50]
          :intcode/input []
          :intcode/output []
          :intcode/pc 0}
         (sut/create-computer "1,9,10,3,2,3,11,0,99,30,40,50")))

  (is (= {:intcode/memory [1 9 10 3 99]
          :intcode/input [10]
          :intcode/output []
          :intcode/pc 0}
         (sut/create-computer "1,9,10,3,99" [10]))))

(deftest run-computer-test
  (testing "simple storage"
    (let [final-computer (sut/run-computer
                          (sut/create-computer "8,1,1,3,99"))]
      (is (map? final-computer))
      (is (:intcode/memory final-computer))
      (is (= [8 1 1 1 99]
             (:intcode/memory final-computer)))))

  (testing "addition (op code 1)"
    (let [computer (sut/run-computer
                    (sut/create-computer "1,1,1,2,99"))]
      (is (= [1 1 2 2 99]
             (:intcode/memory computer))))
    (let [computer (sut/run-computer
                    (sut/create-computer "1,5,6,2,99,4,5"))]
      (is (= [1 5 9 2 99 4 5]
             (:intcode/memory computer)))))

  (testing "multiplication (op code 2)"
    (let [computer (sut/run-computer
                    (sut/create-computer "2,5,6,2,99,4,5"))]
      (is (= [2 5 20 2 99 4 5]
             (:intcode/memory computer)))))

  (testing "input (op code 3)"
    (let [computer (sut/run-computer
                    (sut/create-computer "3,3,99,1" [20]))]
      (is (= [3 3 99 20]
             (:intcode/memory computer)))))

  (testing "output (op code 4)"
    (let [computer (sut/run-computer
                    (sut/create-computer "4,3,99,20"))]
      (is (= [4 3 99 20]
             (:intcode/memory computer)))
      (is (= [20]
             (:intcode/output computer))))))

(deftest previous-solutions
  (testing "day 2"
    (let [input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,5,19,23,2,10,23,27,1,27,5,31,2,9,31,35,1,35,5,39,2,6,39,43,1,43,5,47,2,47,10,51,2,51,6,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,1,71,5,75,1,13,75,79,1,6,79,83,2,83,13,87,1,87,6,91,1,10,91,95,1,95,9,99,2,99,13,103,1,103,6,107,2,107,6,111,1,111,2,115,1,115,13,0,99,2,0,14,0"]
      (is (= 4714701
             (-> input
                 sut/create-computer
                 ;; replace position 1 with the value 12 and replace position 2 with the value 2.
                 (assoc-in [:intcode/memory 1] 12)
                 (assoc-in [:intcode/memory 2] 2)
                 sut/run-computer
                 :intcode/memory
                 (get 0))))

      (is (= 19690720
             (-> input
                 sut/create-computer
                 (assoc-in [:intcode/memory 1] 51)
                 (assoc-in [:intcode/memory 2] 21)
                 sut/run-computer
                 :intcode/memory
                 (get 0)))))))
