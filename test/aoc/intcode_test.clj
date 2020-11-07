(ns aoc.intcode-test
  (:require [aoc.intcode :as sut]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest parsing-opcodes
  (testing "opcode 2"
    (is (= {:instruction 2
            :parameter-modes [0 1 0]}
           (sut/parse-opcode 1002)))
    (is (= {:instruction 8
            :parameter-modes [0 1 1]}
           (sut/parse-opcode 11008)))

    (is (= {:instruction 99
            :parameter-modes [0 1 0]}
           (sut/parse-opcode 1099)))))

(deftest parsing-intcode
  (testing "grabbing the last program element"
    (is (= (sut/parse-intcode 4 [1201 3 4 3 99])
           [99])))

  (testing "Opcode 7 stores 1 in 5 when (< second first"
    (is (= [7 1 2 1]
           (sut/execute-intcode [7 1 2 3] [7 1 2 3]))))

  (testing "Opcode 8 stores 1 in 5 when (= second first)"
    (is (= [8 1 1 1]
           (sut/execute-intcode [8 1 1 3] [8 1 1 3])))))

(deftest output-test
  (testing "output does not change memory"
    (is (= [4 3 99]
           (sut/execute-intcode [4 3] [4 3 99])))))

(deftest immediate-mode-testing
  (testing "param fetching with immediate mode"
    (is (= 2
           (sut/get-param-value-with-mode 2 1 [3 4 5 6])))))

(deftest full-program
  (testing "This program outputs 999 for inputs < 8, 1000 if == 8, 1001 otherwise"
    (let [program-memory "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
      (let [computer (-> program-memory
                         sut/tokenize
                         sut/run)]
        (is (= [999] (:intcode/output computer)))
        (is (int? (:intcode/pc computer))))))

  (testing "opcode 99 should just exit"
    (is (= {:intcode/output []
            :intcode/memory [99]
            :intcode/pc 1
            :intcode/input []}
           (sut/run [99])))))

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
             (:intcode/memory final-computer))))))
