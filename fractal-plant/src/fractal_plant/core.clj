(ns fractal-plant.core
  (:require [quil.core :as q]
            [clojure.string :as string]))

(def ^:const num-generations 8)
(def ^:const width 800)
(def ^:const height 800)
(def ^:const angle (q/radians 25))
(def ^:const length 2)
(def ^:const background-color [255 255 255])
(def ^:const stem-color [100 50 0])
(def ^:const leaf-color [0 102 0])
(def ^:const production-rules {\F "FF"
                               \X "F+[[X]-X]-F[-FX]+X"
                               \+ "+"
                               \- "-"
                               \[ "["
                               \] "]"})
(def word (atom "X"))
(def draw-rules (atom {}))

(defn setup []
  (q/frame-rate 30)
  (q/stroke-weight 1)
  (reset! draw-rules {
                      \F (fn []
                           (let [len (* -1 length)]
                             (apply q/stroke stem-color)
                             (q/line 0 0 0 len)
                             (q/translate 0 len)))
                      \X (fn [] )
                      \+ (fn []
                           (q/rotate (* -1 angle)))
                      \- (fn []
                           (q/rotate angle))
                      \[ q/push-matrix
                      \] (fn []
                           (q/no-stroke)
                           (apply q/fill leaf-color)
                           (q/ellipse 0 0 (* 2 length) (* 5 length))
                           (q/pop-matrix))
                      })
  (q/no-loop))

(defn generate [wrd]
  (string/join (map production-rules wrd)))

(defn draw-generation [wrd]
  (apply q/background background-color)
  ;; We don't want transformations and rotations from previous generations to affect future
  ;; generations, so push the current transformation matrix, draw this generation, and pop it
  ;; back off to restore the original.
  (q/push-matrix)
  (dorun (map #((@draw-rules %)) wrd))
  (q/pop-matrix))

(defn draw []
  (q/with-translation [(* width 0.2) (* height 0.9)]        ; Start in the bottom left
    (q/with-rotation [angle]
      (dotimes [_ num-generations]
        (draw-generation @word)
        (reset! word (generate @word)))))
  (q/save "fractal-plant.png"))

(q/defsketch fractal-plant
             :title "Fractal Plant"
             :size [width height]
             :setup setup
             :draw draw
             :features [:keep-on-top])
