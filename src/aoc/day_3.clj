(ns aoc.day-3
  (:require [clojure.set :refer [difference intersection union]]
            [clojure.string :refer [split]]))

;; create sets of all points present in lines, grab intersection, compute manhattan distances

(defn parse-string [input]
  (clojure.string/split input #","))

(defn command-distance [command]
  (Integer/parseInt (clojure.string/join "" (drop 1 command))))

(defn command-direction [command]
  (first command))

(defn abs [n]
  (if (pos? n)
    n
    (- 0 n)))


(defn inclusive-range
  "Returns an inclusive range.
  Should still work with start > end (ie. in reverse)"
  [start end]
  (if (< end start)
    (reverse (range end (inc start)))
    (range start (inc end))))

(defn points-on-line
  "Return a set of points that are found on the (straight) line from a
  to b."
  ([start end distance-along-wire]
   (let [[const diff] (if (= (:x start) (:x end))
                        [:x :y]
                        [:y :x])]
     (map-indexed
      (fn [i n] {diff n const (get start const) :distance-along-wire (+ i distance-along-wire)})
      (->> [(get start diff) (get end diff)]
           (apply inclusive-range))))))

(defn points-from-position [command position distance]
  (let [direction (first command)
        {:keys [x y]} position
        command-distance (command-distance command)
        end (case direction
              \U {:x x :y (+ y command-distance)}
              \D {:x x :y (- y command-distance)}
              \R {:x (+ x command-distance) :y y}
              \L {:x (- x command-distance) :y y})]
    [end (points-on-line position end distance) (+ distance command-distance)]))

(defn points-on-wire
  "Returns a set of points on the wire"
  [wire]
  (into #{} (loop [position {:x 0 :y 0}
                   distance 0
                   line-points []
                   commands (split wire #",")]
              (if (empty? commands)
                line-points
                (let [[new-position new-line-points new-distance]
                      (points-from-position (first commands)
                                            position
                                            distance)]
                  (recur new-position
                         new-distance
                         (union line-points new-line-points)
                         (rest commands)))))))

(defn manhattan-distance [{:keys [x y]}]
  (+
   (abs y)
   (abs x)))

(defn point-intersection [a b]
  (let [common-points (apply intersection
                             (map (fn [x]
                                    (into #{} (map
                                               #(dissoc % :distance-along-wire)
                                               x)))
                                  [a b]))

        points-with-distances (map
                               (fn [{:keys [x y]}]
                                 (let [matching-points (filter
                                                        #(and (= x (:x %))
                                                              (= y (:y %)))
                                                        (concat a b)) ]
                                   (reduce
                                    (fn [acc p]
                                      {:distance-along-wire (+ (:distance-along-wire p)
                                                               (:distance-along-wire acc))
                                       :x x
                                       :y y})
                                    matching-points)))
                               common-points)]
    points-with-distances))

(defn min-by [f coll]
  (let [f-memo (memoize f)]
       (reduce
        (fn [current-min elm]
          (if (< (f-memo elm) (f-memo current-min))
            elm
            current-min))
        coll)))

(defn solution-1 [input]
  (let [wires (split input #"\n")]
    (->> wires
         (map points-on-wire)
         (apply point-intersection)
         (map #(assoc % :manhattan-distance (manhattan-distance %)))
         (filter #(not= 0 (:manhattan-distance %)))
         (min-by :manhattan-distance)
         :manhattan-distance)))

(defn solution-2 [input]
  (let [wires (split input #"\n")]
    (->> wires
         (map points-on-wire)
         (apply point-intersection)
         (map #(assoc % :manhattan-distance (manhattan-distance %)))
         (filter #(not= 0 (:manhattan-distance %)))
         (min-by :distance-along-wire)
         :distance-along-wire)))
