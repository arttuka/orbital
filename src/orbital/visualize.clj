(ns orbital.visualize
  (:require [quil.core :refer :all]
            inkwell.core))

(def screen-w 720)
(def screen-h 720)

(defn setup []
  (background 0)
  (lights))

(defn scale' [x] (/ x 25))

(defn draw-circle [position [r g b]]
  (push-matrix)
  (stroke r g b)
  (apply translate (map scale' position))
  (sphere 5)
  (pop-matrix))

(defn draw-line [position1 position2 [r g b]]
  (stroke r g b)
  (apply line (map scale' (concat position1 position2))))

(defn draw [satellites connections start end route]
  (fn [_]
    (background 0)
    (stroke 255)
    (fill 255 200)
    (push-matrix)
    (translate (* screen-h 0.5) (* screen-h 0.5) 0)
    (rotate-y (* (frame-count) 0.003))
    (sphere (scale' 6371))
    (draw-circle (:position start) [0 255 0])
    (draw-circle (:position end) [0 255 255])
    (doseq [satellite satellites]
      (draw-circle (:position satellite) [0 0 255])
      (doseq [satellite2 (conj satellites start end)
              :when (not= (:name satellite) (:name satellite2))
              :when (contains? connections #{(:name satellite) (:name satellite2)})]
        (draw-line (:position satellite) (:position satellite2)
                   (if (contains? route #{(:name satellite) (:name satellite2)})
                     [255 0 0]
                     [0 0 255]))))
    (pop-matrix)))

(defn visualize [satellites connections start end route]
  (inkwell.core/make-sketch!
    {:title "Satellites"
     :setup setup
     :size [screen-w screen-h]
     :draw (draw satellites connections start end route)
     :renderer :p3d
     :on-close #(System/exit 0)}))