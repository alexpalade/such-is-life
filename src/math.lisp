(cl:in-package :sil-game)

(defun rotate-vec (vec angle)
  (vec2 (- (* (x vec) (cos angle)) (* (y vec) (sin angle)))
        (+ (* (x vec) (sin angle)) (* (y vec) (cos angle)))))

(defun len (vec)
  (sqrt (+ (expt (x vec) 2) (expt (y vec) 2))))
