(cl:in-package :sil-game)

(defclass label (element)
  ((text :initform "Button" :initarg :text :accessor text)
   (action :initform nil :initarg :action :accessor action)
   (origin :accessor origin)
   (height :accessor height)
   (width :accessor width)
   (text-align :initarg :text-align :initform 'center :accessor text-align)
   (update :initform nil :initarg :update :accessor update)))

(defmethod element-act ((game sil-game) (label label))
  (when (update label)
    (funcall (update label) game label)))

(defmethod render ((label label))
  (with-pushed-canvas ()
    (translate-canvas-vec (origin label))
    (draw-text-aligned (vec2 0 0)
                          (width label)
                          *element-base-height*
                          (text label)
                          (text-align label))))
