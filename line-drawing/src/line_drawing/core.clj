(ns line-drawing.core
  (:require [quil.core :as q]))

(def ^:const antiquewhite [34 14 98])

(defn mouse-pressed []
  (q/save "outputSave.png"))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100)
  (q/no-loop))

(defn draw []
  (apply q/background antiquewhite)
  (q/stroke-cap :square)
  (dotimes [_ 5]
    (q/stroke-weight (q/random 50))
    (q/stroke (q/random 50) (q/random 100) (q/random 100))
    (q/line (q/random 50 350) (q/random 50 350) (q/random 50 350) (q/random 50 350))))

(q/defsketch line-drawing
             :title "Line Drawing"
             :size [400 400]
             :setup setup
             :draw draw
             :features [:keep-on-top]
             :mouse-pressed mouse-pressed)
