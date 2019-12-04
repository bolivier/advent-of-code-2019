(ns aoc.day-1
  (:require [aoc.input :as input]))

(defn parse [s]
  (Integer/parseInt s))

(def data (map parse (input/get-input 1)))

(defn mass->fuel [mass]
  (-> mass
      (/ 3)
      int
      (- 2)))

(defn solution-1
  "Given a list of module masses (data) calculate the fuel required to
  pilot the ship with those modules using the mass to fuel formula."
  []
  (->> data
       (map mass->fuel)
       (apply +)))

(defn mass->total-fuel
  "Given a list of module masses (data), calculate the total fuel
  required to pilot the ship.  This fn takes into account the weight
  of the added fuel and adds more fuel to compensate.

  NOTE: it is applied to each individual module first, then summed up."
  [mass]
  (let [fuel-seq (iterate mass->fuel mass)]
    (->> fuel-seq
         (take-while #(< 0 %))
         (drop 1)
         (apply +))))

(defn solution-2 []
  (->> data
       (map mass->total-fuel)
       (apply +)))
