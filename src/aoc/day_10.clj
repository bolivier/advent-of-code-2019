(ns aoc.day-10
  (:require [clojure.string :as str]))

(def asteroid-map ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(defn index-into-map [coll]
  (into {} (map vector (range) coll)))

(defn map-values [f coll]
  (into {} (map
            (fn [[k v]]
              [k (f v)])
            coll)))

(def asteroid-points nil)

(defn get-asteroid-points [asteroid-map]
  (let [points (->> (str/split asteroid-map #"\n")
                    (map-indexed (fn [row-idx row]
                                   (map-indexed
                                    (fn [col-idx value]
                                      [value [col-idx row-idx]])
                                    row)))
                    (mapcat identity) ;; flatten 1 level
                    (filter #(= (first %) \#))
                    (map second)
                    (into #{}))]
    (do (def asteroid-points points)
        points)))

(defn raw-slope [a b]
  (let [[ax ay] a
        [bx by] b]
    (try
     (/ (- by ay)
        (- bx ax))
     (catch Exception _
       nil))))

(defn slope-direction
  ([slope] (slope-direction slope nil nil))
  ([slope [ax ay] [bx by]]
   (if (contains? #{0 nil} slope)
     (cond
       (nil? slope) (slope-direction (- by ay))
       (= 0 slope) (slope-direction (- bx ax)))
     (if (pos? slope)
       :positive
       :negative))))

(defn get-slope
  "Slope is all well and good, but it goes in two directions.  I need to
  know when the slop is 0 or ∞ whether the asteroid is positive or
  negative"
  [a b]  
  (let [slope (raw-slope a b)]
    {:slope slope
     :direction (slope-direction slope a b)}))


(defn solution-1 [asteroid-points]
  (apply max
         (map count
              (map #(into #{} %)
                   (map
                    (fn [a]
                      (let [others (clojure.set/difference asteroid-points #{a})]
                        (map #(get-slope a %) others)))
                    asteroid-points)))))

(solution-1 (get-asteroid-points asteroid-map))
