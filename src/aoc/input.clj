(ns aoc.input)

(defn- read-input-file [day]
  (slurp (str "resources/day_" day ".input")))

(defn get-input [day]
  (clojure.string/split (read-input-file day) #"\n"))

(comment
  (def day 1)
  )
