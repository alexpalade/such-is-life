(cl:in-package :sih)

(defclass actor ()
  ((dest :initform (vec2 200 200) :accessor dest)
   (rest-time :initform (+ 0.5 (/ 1 (+ 0.1 (random 10)))) :accessor rest-time)
   (row :initform nil :accessor row)
   (col :initform nil :accessor col)
   (last-move-time :initform (/ 1 (1+ (random 10))) :accessor last-move-time)))

(defmethod render ((this actor))
  (with-pushed-canvas ()
    (draw-circle (vec2 *cell-half* *cell-half*)
                 20
                 :fill-paint *black*
                 :stroke-paint (vec4 1 0 0 0)
                 :thickness 2)))
