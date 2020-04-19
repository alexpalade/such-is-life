(in-package :sil-game)

(defclass status-bar (element)
  ((population :initarg :population :accessor population)
   (alive :initarg :sick :initform 1 :accessor alive)
   (sick :initarg :sick :initform 1 :accessor sick)
   (dead :initarg :dead :initform 1 :accessor dead)
   (padding-vertical :initform 7 :accessor padding-vertical)
   (update :initform nil :initarg :update :accessor update)))

(defmethod element-act ((game sil-game) (bar status-bar))
  (when (update bar)
    (funcall (update bar) game bar)))

(defmethod render ((bar status-bar))
  (with-pushed-canvas ()
    (translate-canvas-vec (origin bar))
    (let* ((alive (alive bar))
           (sick (sick bar))
           (dead (dead bar))
           (population (+ alive dead))
           (healthy (- alive sick))
           (padding-vertical (padding-vertical bar))
           (width (width bar))
           (height (- *element-base-height* (* 2 padding-vertical)))
           (offset-y (- (/ *element-base-height* 2) (/ height 2)))
           (healthy-width (* width (/ healthy population)))
           (sick-width (* width (/ sick population)))
           (dead-width (* width (/ dead population))))
      (draw-rect (vec2 0 offset-y) width height
                 :stroke-paint *status-bar-stroke-color*
                 :fill-paint nil
                 :thickness 1)
      (draw-rect (vec2 healthy-width offset-y) sick-width height
                 :stroke-paint nil
                 :fill-paint *status-bar-sick-color*)
      (draw-rect (vec2 0 offset-y) healthy-width height
                 :stroke-paint nil
                 :fill-paint *status-bar-healthy-color*)
      (draw-rect (vec2 (+ healthy-width sick-width) offset-y) dead-width height
                 :stroke-paint nil
                 :fill-paint *status-bar-dead-color*))))
