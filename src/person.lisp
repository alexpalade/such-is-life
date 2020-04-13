(cl:in-package :sih)

(defclass person ()
  ((dest :initform (vec2 200 200) :accessor dest)
   (rest-time :initform (+ 0.5 (/ 1 (+ 0.1 (random 10)))) :accessor rest-time)
   (row :initform nil :accessor row)
   (col :initform nil :accessor col)
   (last-move-time :initform (/ 1 (1+ (random 10))) :accessor last-move-time)))

(defmethod render-avatar ((this person) asset)
  (with-pushed-canvas ()
    (let* ((width (image-width asset))
           (height (image-height asset))
           (scale-for (max width height))
           (scale (/ (- *cell-size* *cell-padding*) scale-for))
           (scaled-cell-size (/ *cell-size* scale )))
      (scale-canvas scale scale)
      (draw-image
       (vec2 (- (/ scaled-cell-size 2) (/ width 2))
             (- (/ scaled-cell-size 2) (/ height 2)))
       asset))))

(defmethod render ((this person))
  (render-avatar this :person))
