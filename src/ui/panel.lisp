(cl:in-package :sil-game)

(defparameter *element-base-height* 25)
(defparameter *element-padding* 10)

(defclass panel ()
  ((origin :initarg :origin :accessor origin)
   (padding :initform 0 :initarg :padding :reader padding)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (height-occupied :initform 0 :accessor height-occupied)
   (elements :initarg :elements :initform '() :accessor elements)
   (index :initform 0 :accessor index)))

(defmethod initialize-instance :after ((panel panel) &key)
  (setf (height-occupied panel) (padding panel)))

(defclass element ()
  ((text :initform "Some text" :initarg :text :accessor text)
   (action :initform nil :initarg :action :accessor action)
   (origin :accessor origin)
   (row-span :initform 1 :initarg :row-span :accessor row-span)
   (no-padding :initform nil :initarg :no-padding :accessor no-padding)
   (height :accessor height)
   (width :accessor width)))

(defmethod add-element ((panel panel) element)
  (with-slots (height index padding origin height-occupied) panel
    (let* ((element-height (+ (* (row-span element) *element-base-height*)))
           (element-y (- height height-occupied element-height)))
      (when (and (> index 0) (not (no-padding element)))
        (incf element-height *element-padding*)
        (decf element-y *element-padding*))
      (incf index)
      (incf height-occupied element-height)
      (setf (origin element) (vec2 (x origin) element-y)) ;(gamekit:add origin (vec2 padding element-y)))
      (setf (width element) (- (width panel) (* 2 padding )))
    (push element (elements panel)))))

(defmethod draw-button-with-text (origin width height text fill)
  (with-pushed-canvas ()
    (draw-rect origin width height :fill-paint fill)
    (draw-text-centered origin width height text)))

(defmethod draw-round-button-with-text (origin radius text fill)
  (with-pushed-canvas ()
    (draw-circle (add origin (vec2 radius radius)) radius :fill-paint fill)
    (draw-text-centered origin (* 2 radius) (* 2 radius) text)))

(defmethod draw-text-aligned (origin width height text aligned)
  (cond
    ((equal aligned 'center)
     (draw-text-centered origin width height text))
    ((equal aligned 'left)
     (draw-text-left origin width height text))))

(defmethod draw-text-left (origin width height text)
  (let* ((text-offset-x 0)
         (text-offset-y (- (/ height 2) (/ (text-height text) 2)))
         (text-origin-x (+ (x origin) text-offset-x))
         (text-origin-y (+ (y origin) text-offset-y)))
    (draw-text text (vec2 text-origin-x text-origin-y))))

(defmethod draw-text-centered (origin width height text)
  (let* ((text-offset-x (- (/ width 2) (/ (text-width text) 2)))
         (text-offset-y (- (/ height 2) (/ (text-height text) 3)))
         (text-origin-x (+ (x origin) text-offset-x))
         (text-origin-y (+ (y origin) text-offset-y)))
    (draw-text text (vec2 text-origin-x text-origin-y))))

(defun text-width (text)
  (nth-value 1 (calc-text-bounds text)))

(defun text-height (text)
  (nth-value 2 (calc-text-bounds text)))

(defmethod render ((panel panel))
  (dolist (e (elements panel))
    (render e)))

(defmethod click-event ((panel panel) cursor-pos)
  (dolist (e (elements panel))
    (click-event e cursor-pos)))

(defmethod click-event ((element element) cursor-pos))

(defmethod element-act ((game sil-game) (element element)))

(defmethod panel-act ((game sil-game))
  (dolist (e (elements (panel game)))
    (element-act game e)))
