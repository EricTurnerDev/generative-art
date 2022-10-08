(ns rushing-water.flow-field-2d)

(declare create)
(declare coordinates->index)
(declare index->coordinates)
(declare get-angle)
(declare update-angle)

;; angle-fn takes a row and a column, and returns an angle in radians to use for that
;; position in the flow field.

(defn create
  "Creates a flow field"
  ([angle-fn num-cols num-rows] (create angle-fn num-cols num-rows (* Math/PI 0.25)))
  ([angle-fn num-cols num-rows angle]
   {:num-cols num-cols
    :num-rows num-rows
    :angle-fn angle-fn
    :values   (vec (repeat (* num-cols num-rows) angle))}))

(defn get-angle [flow-field col row]
  (let [index (coordinates->index flow-field col row)]
    (nth (:values flow-field) index)))

(defn update-angles [flow-field]
  "Applies angle-fn to every location in the flow field to get the new angles"
  (let [angle-fn (:angle-fn flow-field)
        values (vec (map-indexed
                      (fn [index _] (apply angle-fn (index->coordinates flow-field index)))
                      (:values flow-field)))]
    (assoc flow-field :values values)))

(defn- coordinates->index [flow-field col row]
  "Get the index into the :values vector from coordinates"
  (+ col (* row (:num-cols flow-field))))

(defn- index->coordinates [flow-field index]
  "Get the coordinates for a value from its index in the :values vector"
  (let [num-cols (:num-cols flow-field)
        row (int (Math/floor (/ index num-cols)))
        col (int (Math/floor (- index (* row num-cols))))]
    [col row]))