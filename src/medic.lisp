(cl:in-package :sih)

(defclass medic (person) ())

(defmethod render ((this medic))
  (render-avatar this :medic))
