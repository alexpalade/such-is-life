(in-package :sil-game)

(defclass separator (element)
  ((row-span :initform 0.2 :initarg :row-span :accessor row-span)))

(defmethod render ((sep separator))
  (with-pushed-canvas ()
    (translate-canvas-vec (origin sep))
    (let* ((sep-height (* (row-span sep) *element-base-height*))
           (sep-y (/ sep-height 2)))
      (draw-line (vec2 0 sep-y) (vec2 (width sep) sep-y) *black*))))
