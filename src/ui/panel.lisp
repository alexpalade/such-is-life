(cl:in-package :sih)

(defparameter *element-base-height* 30)
(defparameter *element-padding* 10)

(defclass panel ()
  ((origin :initarg :origin :accessor origin)
   (padding :initform 0 :initarg :padding :reader padding)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (height-occupied :initform 0 :accessor height-occupied)
   (elements :initarg :elements :initform '() :accessor elements)
   (index :initform 0 :accessor index)))

(defclass element ()
  ((text :initform "Some text" :initarg :text :accessor text)
   (action :initform nil :initarg :action :accessor action)
   (origin :accessor origin)
   (row-span :initform 1 :initarg :row-span :accessor row-span)
   (height :accessor height)
   (width :accessor width)))

(defmethod add-element ((panel panel) element)
  (with-slots (height index padding origin height-occupied) panel
    (let* ((element-height (+ (* (row-span element) *element-base-height*) *element-padding*))
           (element-y (- height height-occupied element-height)))
      (incf index)
      (incf height-occupied element-height)
      (setf (origin element) (vec2 (x origin) element-y)) ;(gamekit:add origin (vec2 padding element-y)))
      (setf (width element) (- (width panel) (* 2 padding )))
    (push element (elements panel)))))

(defmethod render ((panel panel))
  (dolist (e (elements panel))
    (render e)))

(defmethod click-event ((panel panel) cursor-pos)
  (dolist (e (elements panel))
    (click-event e cursor-pos)))

(defmethod click-event ((element element) cursor-pos))

(defmethod render-button-with-text (origin width height text fill)
  (with-pushed-canvas ()
    (draw-rect origin width height :fill-paint fill)
    (let* ((text-width (text-width text))
           (text-offset-x (/ text-width 2))
           (button-center-x (/ width 2))
           (label-origin-x (- button-center-x text-offset-x))
           (text-height (text-height text))
           (text-offset-y (/ text-height 2))
           (button-center-y (/ height 2))
           (label-origin-y (- button-center-y text-offset-y)))
      (draw-text text
                 (trivial-gamekit:add origin
                                      (vec2 label-origin-x label-origin-y))))))

(defmethod render-text-centered (origin width height text)
  (let* ((text-offset-x (- (/ width 2) (/ (text-width text) 2)))
         (text-offset-y (- (/ height 2) (/ (text-height text) 2)))
         (text-origin-x (+ (x origin) text-offset-x))
         (text-origin-y (+ (y origin) text-offset-y)))
    (draw-text text (vec2 text-origin-x text-origin-y))))

(defun text-width (text)
  (nth-value 1 (calc-text-bounds text)))

(defun text-height (text)
  (nth-value 2 (calc-text-bounds text)))
