(ns wispy-lines.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [wispy-lines.vector :as v]
            [wispy-lines.particle :as p]))

(def ^:const width 1200)
(def ^:const height 800)
(def ^:const num-particles 5000)
(def ^:const particle-color [0 0 0 10])
(def ^:const particle-stroke-weight 1)
(def ^:const scale 20)
(def ^:const xoff-step 0.1)
(def ^:const yoff-step 0.1)
(def ^:const zoff-step 0.002)
(def ^:const two-pi (* 2 (Math/PI)))
(def ^:const vector-magnification 0.3)
(def ^:const background-color 255)
(def ^:const frame-rate 60)

(def initial-state {:zoff       0
                    :color particle-color
                    :flow-field []
                    :particles  (for [_ (range num-particles)]
                                  (p/create-2d
                                    (q/random width)
                                    (q/random height)))})

(defn update-particles [particles flow-field]
  (for [particle particles]
    (let [pos (:pos particle)
          cols (Math/floor (/ width scale))
          x (Math/floor (/ (nth pos 0) scale))
          y (Math/floor (/ (nth pos 1) scale))
          idx (+ x (* y cols))
          frc (nth flow-field idx)]
      (p/update-particle particle frc width height))))

;; TODO: Flow field calculations could be optimized. If the number of locations in
;; the flow field occupied by particles is fewer than the size of the flow field, then
;; compute the flow field for a particle's position when updating the particle, and cache
;; the result so other particles at the same location can use the same force without
;; having to re-calculate it.
(defn update-flow-field [noise-fn zoff]
  (let [cols (Math/floor (/ width scale))
        rows (Math/floor (/ height scale))]
    (for [y (range rows)
          x (range cols)]
      (let [xoff (* x xoff-step)
            yoff (* y yoff-step)
            noise (noise-fn xoff yoff zoff)
            angle (* noise two-pi 4)]
        (v/magnify (v/from-angle angle) vector-magnification)))))

(defn setup []
  (q/frame-rate frame-rate)
  (q/background background-color)
  initial-state)

(defn update-state [state]
  (let [flow-field (update-flow-field q/noise (:zoff state))
        particles (update-particles (:particles state) flow-field)]
    (merge state {
                  :zoff       (+ (:zoff state) zoff-step)
                  :flow-field flow-field
                  :particles  particles
                  })))

(defn draw-particles! [particles color]
  (doseq [particle particles]
    (q/stroke color)
    (q/stroke-weight particle-stroke-weight)
    ;(apply q/point (:pos particle))
    (q/line (:prev-pos particle) (:pos particle))))

(defn draw-vector! [v x y]
  (q/stroke 0 50)
  (q/stroke-weight 1)
  (q/push-matrix)
  (q/translate (* x scale) (* y scale))
  (q/rotate (v/heading v))
  (q/line 0 0 scale 0)
  (q/pop-matrix))

(defn draw-flow-field! [flow-field]
  (when (> (count flow-field) 0)
    (let [cols (q/floor (/ (q/width) scale))
          rows (q/floor (/ (q/height) scale))]
      (doseq [y (range 0 rows)
              x (range 0 cols)]
        (let [index (+ x (* y rows))
              vector (nth flow-field index)]
          (draw-vector! vector x y))))))

(defn draw-state! [state]
  ;(println (q/current-frame-rate))
  ;(q/background 255)
  ;(draw-flow-field (:flow-field state))
  (draw-particles! (:particles state) (apply q/color (:color state))))

(q/defsketch wispy-lines
             :title "wispy-lines"
             :size [width height]
             :renderer :p2d
             :setup setup
             :update update-state
             :draw draw-state!
             :features [:keep-on-top]
             :middleware [m/fun-mode])
