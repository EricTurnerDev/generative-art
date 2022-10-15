(ns fractal-plant.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as string]))

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
(def ^:const draw-rules {
                         \F (fn []
                              (let [len (* -1 length)]
                                (apply q/stroke stem-color)
                                (q/line 0 0 0 len)
                                (q/translate 0 len)))
                         \X (fn [])
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

(defn setup []
  (q/frame-rate 30)
  {:word "X"})

(defn generate [wrd]
  (string/join (map production-rules wrd)))

(defn draw-generation [wrd]
  (apply q/background background-color)
  ;; Use push-matrix and pop-matrix to prevent transformations in the current generation from
  ;; affecting future generations.
  (q/push-matrix)
  (dorun (map #((draw-rules %)) wrd))
  (q/pop-matrix))

(defn update-state [state]
  {:word (generate (:word state))})

(defn draw-state [state]
  (q/with-translation [(* width 0.2) (* height 0.9)]        ; Start in the bottom left
                      (q/with-rotation [angle]
                                       (draw-generation (:word state))))
  (q/no-loop))

(defn on-mouse-press [state _]
  (q/start-loop)
  state)

(q/defsketch fractal-plant
             :title "Fractal Plant"
             :size [width height]
             :setup setup
             :update update-state
             :draw draw-state
             :features [:keep-on-top]
             :mouse-pressed on-mouse-press
             :middleware [m/fun-mode])
