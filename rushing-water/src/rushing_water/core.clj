(ns rushing-water.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [rushing-water.flow-field-2d :as ff]))

(def ^:const resolution 15)                                 ; Width and height in pixels of each position in the flow field
(def ^:const image-width 1500)
(def ^:const image-height 1500)
(def ^:const step-length (* image-width 0.001))

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

;(defn angle-fn [_ row]
;  (* (/ row num-rows) Math/PI))

(defn angle-fn [col row]
  (* Math/PI 4 (q/noise (* col 0.01) (* row 0.01))))

(defn setup []
  (q/smooth)
  (q/frame-rate 30)
  (q/color-mode :rgb)
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
  (take num-steps
        (iterate next-curve-point [start-x start-y flow-field])))

(defn draw-curve
  ([start-x start-y num-steps flow-field]
   (draw-curve start-x start-y num-steps flow-field 3))
  ([start-x start-y num-steps flow-field weight]
   (let [points (curve-points start-x start-y num-steps flow-field)]
     (doseq [[x y _] points]
       (q/point x y)))))

(defn draw-border [color]
  (apply q/stroke color)
  (q/stroke-weight 100)
  (q/line 0 0 image-width 0)
  (q/line image-width 0 image-width image-height)
  (q/line image-width image-height 0 image-height)
  (q/line 0 image-height 0 0))

(defn draw-state [state]
  (let [background-color [0 150 (q/random 200 256)]]

    (apply q/background background-color)

    ;(draw-flow-field (:flow-field state) (:resolution state))

    (dotimes [_ 10000]
      (q/stroke-weight (q/random 1 15))
      (q/stroke 0 (q/random 100 200) (q/random 200 256) (q/random 100))
      (draw-curve (q/random 0 image-width) (q/random 0 image-height) (q/random 1000 15000) (:flow-field state)))

    (dotimes [_ 100]
      (q/stroke-weight (q/random 1 5))
      (q/stroke (q/random 256) (q/random 256) 0 (q/random 256))
      (draw-curve (q/random 0 image-width) (q/random 0 image-height) (q/random 1000 15000) (:flow-field state)))

    (draw-border [255 255 255])

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
