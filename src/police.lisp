(cl:in-package :sih)

(defclass police (person) ())

(defmethod render ((this police))
  (with-pushed-canvas ()
    (let* ((width (image-width :police))
           (height (image-height :police))
           (scale-for (max width height))
           (scale (/ (- *cell-size* *cell-padding*) scale-for))
           (scaled-cell-size (/ *cell-size* scale )))
      (scale-canvas scale scale)
      (draw-image
       (vec2 (- (/ scaled-cell-size 2) (/ width 2))
             (- (/ scaled-cell-size 2) (/ height 2)))
       :police))))
