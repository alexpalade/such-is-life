(cl:in-package :sih)

(defclass killer (person) ())

(defmethod render ((this killer))
  (render-avatar this :killer))
