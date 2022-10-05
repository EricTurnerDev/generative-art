(ns wispy-lines.particle
  (:require [wispy-lines.vector :as v]))

(def ^:const max-velocity 2)

(defn create-2d
  ([] (create-2d 0 0))
  ([x y] (create-2d x y 0 0))
  ([x y vx vy] (create-2d x y vx vy 0 0))
  ([x y vx vy ax ay] {:pos      [x y]
                      :prev-pos [x y]
                      :vel      [vx vy]
                      :acc      [ax ay]}))

(defn- adjust-position-2d [pos max-w max-h]
  "Adjusts the position to stay within the boundaries"
  (let [x (mod (nth pos 0) max-w)
        y (mod (nth pos 1) max-h)]
    [x y]))

(defn- adjust-previous-position-2d [prev-pos curr-pos max-x max-y]
  "Adjusts the previous position so it isn't all the way across the view area when the current position crosses the area boundaries"
  (let [curr-x (nth curr-pos 0)
        curr-y (nth curr-pos 1)
        adj-curr-pos (adjust-position-2d curr-pos max-x max-y)
        adj-curr-x (nth adj-curr-pos 0)
        adj-curr-y (nth adj-curr-pos 1)
        prev-x (nth prev-pos 0)
        prev-y (nth prev-pos 1)
        x (if (or (>= curr-x max-x) (< curr-x 0)) adj-curr-x prev-x)
        y (if (or (>= curr-y max-y) (< curr-y 0)) adj-curr-y prev-y)]
    [x y]))

(defn apply-force [frc particle]
  (let [acc (v/add (:acc particle) frc)]
    (merge particle {:acc acc})))

(defn update-particle
  ([particle max-x max-y] (update-particle particle [0 0] max-x max-y))
  ([particle frc max-x max-y]
   (let [vel (v/limit (v/add (:vel particle) (:acc particle)) max-velocity)
         curr-pos (v/add (:pos particle) vel)
         prev-pos (:pos particle)
         acc (v/mult (:acc particle) 0)
         p (merge particle {:vel vel :pos (adjust-position-2d curr-pos max-x max-y) :prev-pos (adjust-previous-position-2d prev-pos curr-pos max-x max-y) :acc acc})]
     (apply-force frc p))))
