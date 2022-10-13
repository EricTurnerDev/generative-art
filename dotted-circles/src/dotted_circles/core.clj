(ns dotted-circles.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [dotted-circles.map-val :as mv]))

(def ^:const image-width 1200)
(def ^:const image-height 1200)
(def ^:const max-radius 500)
(def ^:const num-dots 100000)
(def ^:const max-dot-radius 10)
(def ^:const color-palette [[222 236 251]
                            [190 218 247]
                            [122 179 239]
                            [54 140 231]
                            [22 102 186]])
(def ^:const sign? true)
(def ^:const seed (rand-int (Integer/MAX_VALUE)))
;(def ^:const seed 0)

(defn setup []
  (q/frame-rate 30)
  (q/random-seed seed)
  (q/color-mode :rgb)
  (q/background 255 255 255)
  {})

(defn update-state [state]
  (merge state {}))

(defn random-position []
  (let [angle (q/random (* 2 Math/PI))
        x (* (q/random max-radius) (q/cos angle))
        y (* (q/random max-radius) (q/sin angle))]
    [x y]))

(defn distance [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defn random-color []
  (nth color-palette (int (q/random (count color-palette)))))

(defn random-dot-size []
  (* 2 (q/random max-dot-radius)))

(defn draw-dot [x y diameter color]
  (apply q/fill color)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
                      (q/ellipse x y diameter diameter)))

(defn sign
  ([author seed] (sign author seed 75))
  ([author seed border-thickness]
            "Adds a signature to the bottom of the image"
            (q/fill 0 0 0)
            (let [date (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") (java.util.Date.))
                  text (format "%s, %s" author date)
                  x (- image-width border-thickness 132)
                  y (+ (- image-height border-thickness) 20)]
              (q/text text x y))
            (let [x (- image-width border-thickness 132)
                  y (+ (- image-height border-thickness) 40)]
              (q/text seed x y))))

(defn draw-state [state]
  (doseq [_ (range num-dots)]
    (let [[x y] (random-position)
          ellipse-color (random-color)
          red (- 255 (mv/map-val (distance x y) 0 max-radius 0 255)) ; Closer to the center has more red
          green (nth ellipse-color 1)
          blue (- 255 red)                                  ; Farther from the center has more blue
          ellipse-dia (random-dot-size)]
      (draw-dot x y ellipse-dia [red green blue])))

  (let [[x y] (random-position)
        color [255 0 0]
        dia (* 2 (random-dot-size))]
    (draw-dot x y dia color))

  (when sign?
    (sign "Eric Turner" (format "%s" seed)))

  (q/save "dotted-circles.png")

  (q/no-loop))


(q/defsketch dotted-circles
             :title "Dotted Circle"
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
