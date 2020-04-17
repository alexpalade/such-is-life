(cl:in-package :sih)

(defclass killer (person)
  ((last-kill-time :initform nil :accessor last-kill-time)))

;(defmethod act ((this killer))
;  (get-neighbors sih this)

(defmethod render ((this killer))
  (render-avatar this :killer))

(defmethod kill-cooldown-ok ((this killer))
  (or
   (null (last-kill-time this))
   (> (- (real-time-seconds) (last-kill-time this)) 1)))
