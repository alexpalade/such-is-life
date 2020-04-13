(cl:in-package :sih)

(defclass policeman (actor) ())

(defmethod render ((this policeman))
  (with-pushed-canvas ()
    (draw-rect (vec2 (- *cell-half* 15) (- *cell-half* 15))
               30
               30
               :fill-paint *black*
               :stroke-paint (vec4 1 0 0 0)
               :thickness 2)))
