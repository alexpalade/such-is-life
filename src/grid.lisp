(cl:in-package :sil-game)

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

          (render-tile this x y :tile)
          (draw-rect (vec2 x y) *cell-size* *cell-size* :thickness *grid-thickness* :stroke-paint *grid-color*))))))


(defmethod render-tile ((this grid) x y asset)
  (with-pushed-canvas ()
    (let* ((width (image-width asset))
           (scale (/ *cell-size* width)))
      (scale-canvas scale scale)
      (draw-image
       (vec2 (/ x scale) (/ y scale))
       asset))))

(defmethod distance-between (row1 col1 row2 col2)
  (+ (abs (- row1 row2)) (abs (- col1 col2))))
