(cl:in-package :sih)

(defclass actor ()
  ((pos :initform (vec2 100 100) :accessor pos)
   (dest :initform (vec2 200 200) :accessor dest)))

(defmethod tick ((this actor))
  (gamekit:draw-text (write-to-string (len (subt (vec2 200 200) (pos this)))) (gamekit:vec2 300 400))
  (setf (pos this) (add (pos this) (rotate-vec (vec2 1 1) (random 360)))))

(defmethod new-dest ((this actor))
  ())

(defmethod render ((this actor))
  (draw-circle (pos this)
               20
               :fill-paint *black*
               :stroke-paint (vec4 1 0 0 0)
               :thickness 2))
