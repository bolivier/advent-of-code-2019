(ns aoc.day-6
  (:require [clojure.string :refer [split]]
            [clojure.set :refer :all]))

(defn parse-orbit-string [s]
  (->> (split s #"\)")
       (map keyword)
       reverse
       (into [])))

(defn parse-input-string [input]
  (->> (split input #"\n")
       (map parse-orbit-string)
       (into {})))

(defn generate-path-to-root [planet orbit-map]
  (loop [path []
         identifier planet]
    (let [next-planet (orbit-map identifier)]
      (if (nil? next-planet)
        path
        (recur (conj path next-planet) next-planet)))))

(defn read-real-input []
  (slurp "resources/day_6.input"))

(defn solution-1 [input]
  (let [orbit-map (parse-input-string input)]
    (->> (keys orbit-map)
         (map #(generate-path-to-root % orbit-map))
         (map count)
         (apply +))))

(defn solution-2 [input]
  (let [orbit-map (parse-input-string input)
        my-path (into #{} (generate-path-to-root :YOU orbit-map))
        san-path (into #{} (generate-path-to-root :SAN orbit-map))
        intersections (intersection my-path san-path)]
    (count (union (difference my-path intersections)
                  (difference san-path intersections)))))

(comment
  (def input "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

  (def input "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

  (def lst [[:I :D]
            [:K :J]
            [:B :COM]
            [:E :D]
            [:D :C]
            [:J :E]
            [:L :K]
            [:F :E]
            [:G :B]
            [:C :B]
            [:H :G]])

  (def orbit-map (into {} lst))
  )
