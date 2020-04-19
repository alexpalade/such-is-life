(cl:in-package :sil-game)

(defclass button (element)
  ())

(defmethod render ((button button))
  (with-pushed-canvas ()
    (translate-canvas-vec (origin button))
    (draw-button-with-text (vec2 0 0)
                             (width button)
                             *element-base-height*
                             (text button)
                             *button-color*)))

(defmethod click-event ((this button) cursor-pos)
  (let ((x (x cursor-pos))
        (y (y cursor-pos)))
    (when
        (and
         (> x (x (origin this)))
         (< x (+ (x (origin this)) (width this)))
         (> y (y (origin this)))
         (< y (+ (y (origin this)) *element-base-height*)))
      (funcall (action this)))))
