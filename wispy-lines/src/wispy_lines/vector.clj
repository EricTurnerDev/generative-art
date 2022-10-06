(ns wispy-lines.vector
  (:require [clojure.core.matrix :as m]))

(defn from-angle
  "Create a vector from an angle in the x y plane (z is always 0)"
  ([angle] (from-angle angle 1))
  ([angle length]
   (let [x (* length (Math/cos angle))
         y (* length (Math/sin angle))
         z 0]                                               ; z is 0 because angle is in the x y plane
     [x y z])))

(defn heading [v]
  "Determine direction of vector v in the x y plane"
  (Math/atan2 (nth v 1) (nth v 0)))                         ; z isn't used here because direction is in the x y plane

(defn magnify [v n]
  (m/mul (m/normalise v) n))

(defn limit [v max]
  "Limit the magnitude of the vector"
  (let [msq (m/magnitude-squared v)]
    (if (> msq (* max max))
      (m/mul (m/div v (Math/sqrt msq)) max)
      v)))

