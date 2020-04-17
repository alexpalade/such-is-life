(cl:in-package :sih)

(defclass person ()
  ((dest :initform (vec2 200 200) :accessor dest)
   (rest-time :initform (+ 0.5 (/ 1 (+ 0.1 (random 10)))) :accessor rest-time)
   (row :initform nil :accessor row)
   (col :initform nil :accessor col)
   (health :initform 100 :accessor health)
   (sick :initform nil :accessor sick)
   (medicine :initform 0 :accessor medicine)
   (last-cough-time :initform nil :accessor last-cough-time)
   (last-move-time :initform (/ 1 (1+ (random 10))) :accessor last-move-time)))

(defmethod become-sick ((this person))
  (when (not (sick this))
    (setf (last-cough-time this) (real-time-seconds))
    (setf (medicine this) 0)
    (setf (sick this) T)))

(defmethod become-healthy ((this person))
  (setf (health this) 100)
  (setf (sick this) nil))

(defmethod cough ((this person))
  (decf (health this) 1)
  (setf (last-cough-time this) (real-time-seconds)))

(defmethod render-avatar ((this person) asset)
  (with-pushed-canvas ()
    (let* ((width (image-width asset))
           (height (image-height asset))
           (scale-for (max width height))
           (scale (/ (- *cell-size* *cell-padding*) scale-for))
           (scaled-cell-size (/ *cell-size* scale))
           (scale-no-padding (/ *cell-size* scale-for))
           (scaled-cell-size-no-padding (/ *cell-size* scale-no-padding)))
      ;(with-pushed-canvas ()
      ;  (scale-canvas scale-no-padding scale-no-padding)
      ;  (when (sick this)
      ;    (draw-image
      ;     (vec2 (- (/ scaled-cell-size-no-padding 2) (/ width 2))
      ;           (- (/ scaled-cell-size-no-padding 2) (/ height 2)))
      ;     :biohazard)))

      (when (sick this)
        (let ((alpha (+ 0.5 (- 0.5 (/ (health this) 100)))))
          (draw-rect (vec2 0 0) *cell-size* *cell-size*
                     :fill-paint (vec4 0.9 0.1 0.1 alpha))))

      (scale-canvas scale scale)
      (draw-image
       (vec2 (- (/ scaled-cell-size 2) (/ width 2))
             (- (/ scaled-cell-size 2) (/ height 2)))
       asset))))

(defmethod render ((this person))
  (render-avatar this :person))
