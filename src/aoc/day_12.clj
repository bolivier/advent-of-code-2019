(ns aoc.day-12
  (:require [aoc.utils :refer [parse-int]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [midje.sweet :as m]))

(s/def ::x number?)
(s/def ::y number?)
(s/def ::z number?)
(s/def ::position (s/keys :req-un [::x ::y ::z]))
(s/def ::velocity (s/keys :req-un [::x ::y ::z]))
(s/def ::moon (s/keys :req-un [::position ::velocity]))

(defn drop-ends [s]
  (drop 1 (drop-last 1 s)))

(defn parse-position [input]
  (let [facts-only-input (str/split (str/join "" (drop-ends input)) #", ?")
        convert-format (fn [[k v]] [(keyword k) (parse-int v)])]
    (->> facts-only-input
         (map #(str/split % #"="))
         (map convert-format)
         (into {}))))

(def starting-velocity
  {:x 0 :y 0 :z 0})

(defn parse-moon-input [input]
  (let [position (parse-position input)
        velocity starting-velocity]
    {:position position :velocity velocity}))

(m/fact "moons are parsed correctly"
        (parse-moon-input "<x=-1, y=0, z=2>") => {:position {:x -1 :y 0 :z 2}
                                                  :velocity {:x 0 :y 0 :z 0}})
(defn abs [n]
  (if (pos? n)
    n
    (- 0 n)))

(defn- energy-calculation [coll]
  (apply + (map abs (vals coll))))

(defn potential-energy [{:keys [position]}]
  (energy-calculation position))

(m/fact "Potential energy is the sum of abs positions"
        (potential-energy {:position {:x 1 :y -2 :z -10}}) => 13)

(defn kinetic-energy [{:keys [velocity]}]
  (energy-calculation velocity))

(defn energy-for-moon [moon]
  (* (potential-energy moon)
     (kinetic-energy moon)))

(m/fact "moon energy is potential times kinetic"
        (energy-for-moon {:velocity {:x 1 :y 0 :z -1}
                          :position {:x 5 :y -4 :z -1}}) => 20)

(defn standardize-magnitude [n]
  (cond
    (pos? n) 1
    (neg? n) -1
    :else 0))

(defn combine-vectors [a b]
  (into {} (map
            (fn [[k _]]
              [k (+ (get a k)
                    (get b k))])
            starting-velocity)))

(defn add-gravity-velocity-to-moon [moon-base moon-compare]
  (let [moon-base-position (:position moon-base)
        moon-compare-position (:position moon-compare)
        zipped-coordinates (map vector (vals moon-compare-position) (vals moon-base-position))
        deltas (->> zipped-coordinates
                    (map #(apply - %))
                    (map standardize-magnitude)
                    (map vector [:x :y :z])
                    (into {}))]
    (assoc moon-base :velocity (combine-vectors deltas (:velocity moon-base)))))

(defn pprint-moon [{:keys [position velocity]}]
  (str "pos="
       "<x=" (get position :x)
       ", y=" (get position :y)
       ", z=" (get position :z)
       ">, "
       "vel=<"
       "x=" (get velocity :x)
       ", y=" (get velocity :y)
       ", z=" (get velocity :z)
       ">"))

(for [a (take 1 system)
      b system
      :when (not= a b)]
  (add-gravity-velocity-to-moon a b))

(comment
  (def moon-base (parse-moon-input "<x=-1, y=0, z=2>"))
  (def moon-compare (parse-moon-input "<x=1, y=2, z=0>"))


  (def a (:velocity moon-base))
  (def b (:position moon-compare))

  (def position {:x 1 :y 2 :z -3})
  (parse-moon-input input)

  (def system (map parse-moon-input
                   (str/split "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>" #"\n")))

  )

