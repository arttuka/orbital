(ns orbital.core
  (:gen-class)
  (:require [astar.core :as a*]
            [clojure.java.io :refer [reader]]
            [quil.core :refer :all]
            [orbital.vector :refer :all]
            [orbital.visualize :refer [visualize]])
  (:import java.lang.Math))

(def R 6371)

(defn ->cartesian
  "Returns the satellite with its position in cartesian coordinates (origin is in the center of the earth)"
  [satellite]
  (let [{:keys [name lat lon alt]} satellite
        inclination (Math/toRadians (- 90 lat))
        azimuth (Math/toRadians (mod (+ 360 lon) 360))
        radius (+ alt R)]
    {:name name
     :position [(* radius (Math/sin inclination) (Math/cos azimuth))
                (* radius (Math/sin inclination) (Math/sin azimuth))
                (* radius (Math/cos inclination))]}))

(defn read-file [file]
  (let [[seed-line & more] (clojure.string/split-lines (slurp (reader file)))
        data (butlast more)
        route-line (last more)
        seed (second (re-matches #"^#SEED: (.*)$" seed-line))
        satellites (for [line data]
                     (->
                       (zipmap [:name :lat :lon :alt] (clojure.string/split line #","))
                       (update :lat #(Double/parseDouble %))
                       (update :lon #(Double/parseDouble %))
                       (update :alt #(Double/parseDouble %))
                       ->cartesian))
        route (zipmap [:lat1 :lon1 :lat2 :lon2] (->>
                                                 (clojure.string/split route-line #",")
                                                 next
                                                 (map #(Double/parseDouble %))))
        start (->cartesian {:name "START", :lat (:lat1 route), :lon (:lon1 route), :alt 0})
        end (->cartesian {:name "END", :lat (:lat2 route), :lon (:lon2 route), :alt 0})]
    {:seed seed, :satellites satellites, :start start, :end end}))

(defn visible?
  "Calculates if two points are visible to each other."
  [p1 p2]
  (let [; Unit vector in the direction of the line from p1 to p2
        n (unit p1 p2)
        ; Length of projection of line from origin to p1 onto line from p1 to p2
        ; This is the distance from p1 to the closest point to origin (center of Earth) on the line from p1 to p2
        closest-point-length (dot p1 n)
        closest-point (minus p1 (mult n closest-point-length))]
    (or
      ; The closest point is not between p1 and p2 => the line segment doesn't go through the earth between p1 and p2
      (not (>= 0 closest-point-length (- (length (minus p2 p1)))))
      ; Or the closest point is outside Earth => the line segment doesn't go through the earth at all
      (< R (length closest-point)))))

(defn adjacent
  "Returns a function from a satellite to a list of all visible satellites."
  [satellites]
  (fn [satellite]
    (for [s satellites
          :when (not= satellite s)
          :when (visible? (:position s) (:position satellite))]
      s)))

(defn distance
  "Returns the distance between two satellites plus 100000 (to prefer lesser amount of hops over smaller distance).
  Used as edge weight for the A* algorithm."
  [s1 s2]
  (+ 10000 (length (minus (:position s2) (:position s1)))))

(defn h
  "Returns a heuristic function for the A* algorithm. Returns the straight-line distance from a satellite to the end point"
  [end]
  (fn [satellite]
    (length (minus (:position end) (:position satellite)))))

(defn -main
  [file]
  (let [{:keys [seed satellites start end]} (read-file file)
        all-points (conj satellites start end)
        route (cons start (a*/route (adjacent all-points) distance (h end) start end))]
    (println "Seed:" seed)
    (println (clojure.string/join \, (map :name route)))
    (let [connections (into #{} (for [s1 all-points
                                      s2 all-points
                                      :when (not= s1 s2)
                                      :when (visible? (:position s1) (:position s2))]
                                  #{(:name s1) (:name s2)}))
          route (into #{} (for [[s1 s2] (partition 2 1 route)]
                            #{(:name s1) (:name s2)}))]
      (visualize satellites connections start end route))))
