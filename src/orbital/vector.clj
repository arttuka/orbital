(ns orbital.vector
  "Operations for vectors and points in 3D space"
  (:import java.lang.Math))

(defn dot
  "Calculates the dot product of two vectors"
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(defn length
  "Calculates the length of a vector"
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn minus
  "Calculates the difference of two vectors"
  [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])

(defn mult
  "Calculates the product of a vector and a number"
  [[x y z] n]
  [(* x n) (* y n) (* z n)])

(defn unit
  "Returns a unit vector for line between two points"
  [[x1 y1 z1] [x2 y2 z2]]
  (let [line [(- x2 x1) (- y2 y1) (- z2 z1)]]
    (mult line (/ (length line)))))