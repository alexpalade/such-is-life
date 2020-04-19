(cl:in-package :sil-game)

(defparameter *decrease-button-offset-x* 80)

(defclass adjuster (element)
  ((allowed-values :initarg :allowed-values :initform (error "Adjuster element needs allowed values") :accessor allowed-values)
   (current-value :initarg :current-value :initform (error "Adjuster element needs current value") :accessor current-value)
   (text :initarg :text :initform "Some text" :accessor text)
   (row-span :initform 1 :accessor row-span)
   (action :initform nil :initarg action :accessor action)))

(defmethod render ((a adjuster))
  (with-pushed-canvas ()
    (translate-canvas-vec (origin a))

    (draw-text-left (vec2 0 0)
                    (width a)
                    *element-base-height*
                    (text a))

    (draw-round-button-with-text (vec2 (- (width a) *element-base-height*) 0)
                             (/ *element-base-height* 2)
                             "+"
                             *button-color*)

    (let* ((text (write-to-string (current-value a)))
           (text-offset-x (+ *decrease-button-offset-x* *element-base-height*)))
      (draw-text-centered (vec2 text-offset-x 0)
                          (- (- (width a) *element-base-height*)
                             text-offset-x)
                          *element-base-height*
                          text)

      (draw-round-button-with-text (vec2 *decrease-button-offset-x*
                                   0)
                             (/ *element-base-height* 2)
                             "-"
                             *button-color*))))


(defmethod click-event ((this adjuster) cursor-pos)
  (let ((x (x cursor-pos))
        (y (y cursor-pos)))
    (when
        (and
         (> x (+ (x (origin this)) *decrease-button-offset-x*))
         (< x (+ (x (origin this)) *decrease-button-offset-x* *element-base-height*))
         (> y (y (origin this)))
         (< y (+ (y (origin this)) *element-base-height*)))
      (previous-value this))
    (when
        (and
         (> x (+ (x (origin this)) (- (width this) *element-base-height*)))
         (< x (+ (x (origin this)) (width this)))
         (> y (y (origin this)))
         (< y (+ (y (origin this)) *element-base-height*)))
      (next-value this))))

(defmethod next-value ((this adjuster))
  (let* ((current-value (current-value this))
         (index (position current-value (allowed-values this))))
    (when (and
           index
           (< index (1- (length (allowed-values this)))))
      (setf (current-value this) (nth (1+ index) (allowed-values this)))
      (when (action this)
        (funcall (action this) (current-value this))))))

(defmethod previous-value ((this adjuster))
  (let* ((current-value (current-value this))
         (index (position current-value (allowed-values this))))
    (when (and
           index
           (> index 0))
      (setf (current-value this) (nth (1- index) (allowed-values this)))
      (when (action this)
        (funcall (action this) (current-value this))))))
