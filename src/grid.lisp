(cl:in-package :sih)

(defclass grid ()
  ((rows :initform (error "grid needs row count") :initarg :rows :reader rows)
   (cols :initform (error "grid needs column count") :initarg :cols :reader cols)
   (cell-size :initform (error "grid needs cell size") :initarg :cell-size :reader cell-size)))

(defmethod render ((this grid))
  (with-pushed-canvas ()
    (dotimes (row (rows this))
      (dotimes (col (cols this))
        (let ((x (* col *cell-size*))
              (y (* row *cell-size*)))
          (draw-rect (vec2 x y) *cell-size* *cell-size* :stroke-paint *black*))))))

;(defclass gridpos ()
;  ((row :initform (error "cell needs row pos") :initarg :row :reader row)
;   (col :initform (error "cell needs col pos") :initarg :col :reader col)))
