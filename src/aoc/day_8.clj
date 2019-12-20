(ns aoc.day-8
  (:require [aoc.utils :refer [parse-int]]))

(def width 25)
(def height 6)

(defn partition-into-layers [vals]
  (partition (* width height) vals))

(defn parse-input [input-str]
  (map (comp parse-int str) input-str))

(def input (slurp "resources/day_8.input"))

(defn partition-into-1d-layers [vals]
  (partition (* height width) vals))

(def black 0)
(def white 1)
(def transparent 2)

(defn zip-layers [layers]
  (apply map (conj layers vector)))

(defn reduce-to-visual-layer [layers]
  (let [zipped-layers (zip-layers layers)]
   (map
    (fn [zipped-layer]
      (->> zipped-layer
           (drop-while #(= 2 %))
           first))   
    zipped-layers)))

(defn frequencies-by-layer [input]
  (let [vals (parse-input input)]
    (map frequencies (partition-into-1d-layers vals))))


(defn index-of-min-zeros [frequencies]  
  (first
   (apply min-key
          (conj
           (map-indexed (fn [i m]
                          [i ((some-fn #(% 0) (constantly 0)) m)])
                        frequencies)
           (fn [[i c]] c)))))

(defn solution-1 [input]
  (let [freq (frequencies-by-layer input)
        layer-idx (index-of-min-zeros freq)
        layer (nth freq layer-idx)]
    (* (layer 2)
       (layer 1))))

(defn solution-2 [input]
  (let [layers (partition-into-1d-layers (parse-input input))
        visual-layer (reduce-to-visual-layer layers)]
    (partition width (map
                      (fn [c]
                        (case c
                          0 " "
                          1 "█"))
                      visual-layer))))

(comment
  (def input "123456789012")
  (partition-into-1d-layers (parse-input input))
  )
