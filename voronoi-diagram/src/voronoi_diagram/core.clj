(ns voronoi-diagram.core
  (:require [quil.core :as q]))

(declare voronoi-diagram)

(def palette (range 210 270))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100))

(defn create-point
  "Creates a point and assigns it the ith hue from the palette."
  [i]
  (let [x (int (q/random (q/width)))
        y (int (q/random (q/height)))
        hue (nth palette i)]
    [x y hue]))

(defn create-points
  [n]
  (map create-point (range n)))

(defn euclidean-distance
  "Calculates the Euclidean distance between two locations."
  [p1 p2 q1 q2]
  (Math/sqrt (+ (Math/pow (- q1 p1) 2) (Math/pow (- q2 p2) 2))))

(defn nearest-point
  "Finds the point closest to a location."
  [x y points]
  (first (sort-by (fn [[px py _]] (euclidean-distance x y px py)) points)))

(defn draw-points
  "Draws the points in black."
  [points]
  (q/stroke-weight 5)
  (q/stroke 0 0 0)
  (doseq [[x y _] points]
    (q/point x y)))

(defn draw-cells
  "Draws every pixel in the image using the color of the nearest point."
  [points]
  (q/stroke-weight 1)                                       ; 1 px
  (dotimes [x (q/width)]
    (dotimes [y (q/height)]
      (let [[_ _ hue] (nearest-point x y points)]
        (q/stroke hue 100 100)
        (q/point x y)))))

(defn draw []
  (q/background 0 0 100)
  (let [points (create-points (count palette))]
    (draw-cells points)
    #_(draw-points points))
  (q/no-loop))

(defn mouse-pressed
  "Draw another image when the mouse is pressed."
  []
  (q/start-loop))

(defn key-pressed
  "Save the image to voronoi-diagram.png when the S key is pressed."
  []
  (when (or (= (q/key-as-keyword) :s) (= (q/key-as-keyword) :S))
    (q/save "voronoi-diagram.png")))

(q/defsketch voronoi-diagram
             :title "Voronoi Diagram"
             :size [1000 1000]
             :setup setup
             :draw draw
             :mouse-pressed mouse-pressed
             :key-pressed key-pressed
             :features [:keep-on-top])
