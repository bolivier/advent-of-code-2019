(ns aoc.day-7
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::phase-setting #{1 2 3 4})
(s/def ::phase-configuration (s/and
                              (s/coll-of ::phase-setting
                                         :distinct true)
                              #(= 4 (count %))))

(def possible-configurations
  (for [a (range 1 5)
        b (range 1 5)
        c (range 1 5)
        d (range 1 5)
        e (range 1 5)]
    [a b c d e]))

