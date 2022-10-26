(ns voronoi-diagram.core
  (:require [quil.core :as q])
  (:import (java.text SimpleDateFormat)
           (java.util Date)))

(declare voronoi-diagram)

(def ^:const seed (rand-int (Integer/MAX_VALUE)))
(def ^:const palette (range 210 270))
(def ^:const white [0 0 100])

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100)
  (q/random-seed seed)
  (q/text-font (q/create-font "DejaVu Sans" 11 true)))

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

(defn draw-border
  ([] (draw-border white))
  ([color] (draw-border color 75))
  ([color thickness]
   (apply q/stroke color)
   (q/rect 0 0 (q/width) thickness)                        ; Top
   (q/rect (- (q/width) thickness) 0 thickness (q/height)) ; Right
   (q/rect 0 (- (q/height) thickness) (q/width) thickness) ; Bottom
   (q/rect 0 0 thickness (q/height))                       ; Left
   ))

(defn sign
  "Adds a signature to the bottom of the image."
  ([author seed] (sign author seed 75))
  ([author seed border-thickness]
   (q/fill 0 0 0)
   (let [date (.format (SimpleDateFormat. "MM/dd/yyyy") (Date.))
         text (format "%s, %s" author date)
         x (- (q/width) border-thickness 132)
         y (+ (- (q/height) border-thickness) 20)]
     (q/text text x y))
   (let [x (- (q/width) border-thickness 132)
         y (+ (- (q/height) border-thickness) 40)]
     (q/text seed x y))))

(defn draw []
  (q/background 0 0 100)
  (let [points (create-points (count palette))
        border-thickness 50]
    (draw-cells points)
    (draw-border white border-thickness)
    (sign "Eric Turner" (str seed) border-thickness)
    #_(draw-points points))
  (q/no-loop))

(defn mouse-pressed
  "Draw another image when the mouse is pressed."
  []
  (q/start-loop))

(defn key-pressed
  "Save the image to voronoi-diagram.png when the S key is pressed."
  []
  (let [key (q/key-as-keyword)]
    (when (or (= key :s) (= key :S))
      (q/save "voronoi-diagram.png"))))

(q/defsketch voronoi-diagram
             :title "Voronoi Diagram"
             :size [1000 1000]
             :setup setup
             :draw draw
             :mouse-pressed mouse-pressed
             :key-pressed key-pressed
             :features [:keep-on-top])
