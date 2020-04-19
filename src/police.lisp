(cl:in-package :sil-game)

(defclass police (person) ())

(defmethod render ((this police))
  (render-avatar this :police))
