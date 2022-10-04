(ns wispy-lines.vector)

(defn from-angle
  ([angle] (from-angle angle 1))
  ([angle length]
   (let [x (* length (Math/cos angle))
         y (* length (Math/sin angle))
         z 0]
     [x y z])))

(defn heading [v]
  "Determine direction of vector v"
  (Math/atan2 (nth v 1) (nth v 0)))

(defn add [& args]
  "Add the vectors"
  (when (seq args)
    (apply mapv + args)))

(defn mag-sq [v]
  "Compute the squared magnitude of the vector v"
  (apply + (map #(* % %) v)))

(defn mag [v]
  "Compute the magnitude of the vector v"
  (Math/sqrt (mag-sq v)))

(defn mult [v n]
  "Multiply vector v by n"
  (mapv #(* % n) v))

(defn div [v n]
  "Divide vector v by n"
  (mapv #(/ % n) v))

(defn normalize [v]
  "Normalize the vector to length 1 (unit vector)"
  (let [len (mag v)]
    (if (not= 0 len)
      (mult v (/ 1 len))
      v)))

(defn magnify [v n]
  (mult (normalize v) n))

(defn limit [v max]
  "Limit the magnitude of the vector"
  (let [msq (mag-sq v)]
    (if (> msq (* max max))
      (mult (div v (Math/sqrt msq)) max)
      v)))

