(cl:in-package :sih)

(defclass policeman (actor)
  ())

(defmethod render ((this policeman))
  (draw-rect (pos this)
             10
             10
             :fill-paint *black*
             :stroke-paint (vec4 1 0 0 0)
             :thickness 2))
