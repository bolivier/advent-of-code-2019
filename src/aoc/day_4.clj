(ns aoc.day-4
  (:require [clojure.string :refer [split]]))

(defn some? [pred coll]
  (boolean (some pred coll)))

(defn six-digit? [n]
  (< 99999 n 999999))

(defn two-adjacent-same? [n]
  (let [coll (split (str n) #"")
        matches (map (fn [[a b]] (= a b)) (map vector coll (rest coll)))]
    (some? identity matches)))

(defn only-two-adjacent-same? [n]
  (let [coll (split (str n) #"")
        adjacent-counts (loop [remaining coll
                               data []]
                          (if (empty? remaining)
                            data
                            (let [[front back] (split-with #(= (first remaining) %)
                                                           remaining)]
                              (recur
                               back
                               (conj data [(first remaining) (count front)])))))]
    (some?
     (fn [[_ count]] (= 2 count))
     adjacent-counts)))

(defn monotonically-increasing? [n]
  (let [coll (split (str n) #"")]
    (apply <= (map #(Integer/parseInt %) coll))))

(defn match [n]
  (every? identity
          (map
           (fn [f] (f n))
           [six-digit?
            two-adjacent-same?
            monotonically-increasing?])))

(def puzzle-range-start 236491)
(def puzzle-range-end 713787)

(defn solution-1 []
  (->> (range puzzle-range-start puzzle-range-end)
       (filter match)
       count))

(defn solution-2 []
  (->> (range puzzle-range-start puzzle-range-end)
       (filter #(and (match %) (only-two-adjacent-same? %)))
       count))
