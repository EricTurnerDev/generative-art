(ns rushing-water.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [rushing-water.flow-field-2d :as ff]))

(def ^:const resolution 15)                                 ; Width and height in pixels of each position in the flow field
(def ^:const image-width 1500)
(def ^:const image-height 1500)
(def ^:const step-length (* image-width 0.001))
(def ^:const line-iterations 10000)
(def ^:const draw-border? true)
(def ^:const draw-flow-field? false)
(def ^:const sign? true)
(def ^:const border-thickness 60)                           ; Should be no less than 50
(def ^:constant seed (rand-int (Integer/MAX_VALUE)))
;(def ^:constant seed 0)                                    ; Specify a seed to re-create an image

(def ^:const left-x 0)
(def ^:const right-x image-width)
(def ^:const top-y 0)
(def ^:const bottom-y image-height)
(def ^:const num-cols (int (/ (- right-x left-x) resolution)))
(def ^:const num-rows (int (/ (- bottom-y top-y) resolution)))

(defn center-pixel [col row resolution]
  "Calculate the center pixel for a cell at col row, given pixels-per-cell number of pixels per cell."
  (let [x (int (+ (* col resolution) (/ resolution 2)))
        y (int (+ (* row resolution) (/ resolution 2)))]
    [x y]))

(defn angle-fn [col row]
  (* Math/PI 4 (q/noise (* col 0.01) (* row 0.01))))

(defn setup []
  (q/smooth)
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/noise-seed seed)
  (q/random-seed seed)
  (q/text-font (q/create-font "DejaVu Sans" 11 true))
  ; setup function returns initial state.
  {:resolution resolution
   :flow-field (ff/update-angles (ff/create angle-fn num-cols num-rows))})

(defn update-state [state]
  (let [ff (ff/update-angles (:flow-field state))]
    (assoc state :flow-field ff)))

(defn draw-flow-field [flow-field resolution]
  (q/stroke 0 0 0)
  (q/stroke-weight 1)
  (doseq [row (range (:num-rows flow-field))
          col (range (:num-cols flow-field))]
    (let [angle (ff/get-angle flow-field col row)
          [x1 y1] (center-pixel col row resolution)
          x2 (+ x1 (* (Math/cos angle) 10))
          y2 (+ y1 (* (Math/sin angle) 10))]
      (q/ellipse x1 y1 2 2)
      (q/line x1 y1 x2 y2))))

(defn next-curve-point [[x y flow-field]]
  "Calculates the next point on the curve to draw"
  (let [x-offset (- x left-x)
        y-offset (- y top-y)
        column-index (int (/ x-offset resolution))
        row-index (int (/ y-offset resolution))
        angle (ff/get-angle flow-field column-index row-index)
        x-step (* step-length (Math/cos angle))
        y-step (* step-length (Math/sin angle))
        next-x (+ x x-step)
        next-y (+ y y-step)]
    (when (< angle 0) (println angle))
    (if (and (< -1 next-x (* (:num-cols flow-field) resolution))
             (< -1 next-y (* (:num-rows flow-field) resolution)))
      [next-x next-y flow-field]
      [x y flow-field])))

(defn curve-points [start-x start-y num-steps flow-field]
  "Lazy sequence of points on a curve through a flow field"
  (take num-steps
        (iterate next-curve-point [start-x start-y flow-field])))

(defn draw-curve
  "Draws a curve through a flow field"
  ([color start-x start-y num-steps flow-field]
   (draw-curve color start-x start-y num-steps flow-field 3))
  ([color start-x start-y num-steps flow-field weight]
   (let [points (curve-points start-x start-y num-steps flow-field)]
     (q/stroke-weight weight)
     (doseq [[x y _] points]
       (apply q/stroke color)
       (q/point x y)))))

(defn draw-border
  ([color] (draw-border color 100))
  ([color thickness]
   (apply q/stroke color)
   (q/rect 0 0 image-width thickness)                       ; Top
   (q/rect (- image-width thickness) 0 thickness image-height) ; Right
   (q/rect 0 (- image-height thickness) image-width thickness) ; Bottom
   (q/rect 0 0 thickness image-height)                      ; Left
   ))

(defn sign [author date seed border-thickness]
  "Adds a signature to the bottom of the image"
  (q/fill 0 0 0)
  (let [text (format "%s, %s" author date)
        x (- image-width border-thickness 132)
        y (+ (- image-height border-thickness) 20)]
    (q/text text x y))
  (let [x (- image-width border-thickness 132)
        y (+ (- image-height border-thickness) 40)]
    (q/text seed x y)))

(defn blueish []
  "Create a random blueish/greenish color"
  [0 (q/random 100 200) (q/random 200 256) (q/random 100)])

(defn reddish []
  "Create a random reddish/greenish color"
  [(q/random 256) (q/random 256) 0 (q/random 256)])

(defn draw-state [state]
  (let [background-color [0 150 (q/random 200 256)]]

    (apply q/background background-color)

    (dotimes [_ line-iterations]
      (let [color (blueish)
            weight (q/random 1 15)
            x (q/random image-width)
            y (q/random image-height)
            steps (q/random 1000 15000)]
        (draw-curve color x y steps (:flow-field state) weight))
      )

    (dotimes [_ (int (* line-iterations 0.02))]
      (let [color (reddish)
            weight (q/random 1 5)
            x (q/random 0 image-width)
            y (q/random 0 image-height)
            steps (q/random 1000 15000)]
        (draw-curve color x y steps (:flow-field state) weight)))

    (when draw-flow-field?
      (draw-flow-field (:flow-field state) (:resolution state)))

    (when draw-border?
      (draw-border [255 255 255] border-thickness))

    (when sign?
      (sign
        "Eric Turner"
        (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") (java.util.Date.))
        (format "%s" seed)
        border-thickness))

    (q/save "rushing-water.png")

    (q/no-loop))
  )


(q/defsketch rushing-water
             :title "Rushing Water"
             :size [image-width image-height]
             ; setup function called only once, during sketch initialization.
             :setup setup
             ; update-state is called on each iteration before draw-state.
             :update update-state
             :draw draw-state
             :features [:keep-on-top]
             ; This sketch uses functional-mode middleware.
             ; Check quil wiki for more info about middlewares and particularly
             ; fun-mode.
             :middleware [m/fun-mode])
