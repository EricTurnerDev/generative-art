(ns wispy-lines.particle
  (:require [clojure.core.matrix :as m]
            [wispy-lines.vector :as v]))

(def ^:const max-velocity 2)

(defn create-particle
  ([] (create-particle 0 0 0))
  ([x y z] (create-particle x y z 0 0 0))
  ([x y z vx vy vz] (create-particle x y z vx vy z 0 0 0))
  ([x y z vx vy vz ax ay az] {:pos      [x y z]
                      :prev-pos [x y z]
                      :vel      [vx vy vz]
                      :acc      [ax ay az]}))

(defn- adjust-position [pos max-w max-h]
  "Adjusts the position to stay within the boundaries"
  (let [x (mod (nth pos 0) max-w)
        y (mod (nth pos 1) max-h)
        z (nth pos 2)]
    [x y z]))

(defn- adjust-previous-position [prev-pos curr-pos max-x max-y]
  "Adjusts the previous position so it isn't all the way across the view area when the current position crosses the area boundaries"
  (let [curr-x (nth curr-pos 0)
        curr-y (nth curr-pos 1)
        curr-z (nth curr-pos 2)
        adj-curr-pos (adjust-position curr-pos max-x max-y)
        adj-curr-x (nth adj-curr-pos 0)
        adj-curr-y (nth adj-curr-pos 1)
        prev-x (nth prev-pos 0)
        prev-y (nth prev-pos 1)
        x (if (or (>= curr-x max-x) (< curr-x 0)) adj-curr-x prev-x)
        y (if (or (>= curr-y max-y) (< curr-y 0)) adj-curr-y prev-y)
        z curr-z]
    [x y z]))

(defn apply-force [frc particle]
  (let [acc (m/add (:acc particle) frc)]
    (merge particle {:acc acc})))

(defn update-particle
  ([particle max-x max-y] (update-particle particle [0 0] max-x max-y))
  ([particle frc max-x max-y]
   (let [vel (v/limit (m/add (:vel particle) (:acc particle)) max-velocity)
         curr-pos (m/add (:pos particle) vel)
         prev-pos (:pos particle)
         acc (m/mul (:acc particle) 0)
         p (merge particle {:vel vel :pos (adjust-position curr-pos max-x max-y) :prev-pos (adjust-previous-position prev-pos curr-pos max-x max-y) :acc acc})]
     (apply-force frc p))))
