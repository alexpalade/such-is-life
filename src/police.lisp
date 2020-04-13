(cl:in-package :sih)

(defclass police (person) ())

(defmethod render ((this police))
  (render-avatar this :police))
