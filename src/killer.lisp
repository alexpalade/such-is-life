(cl:in-package :sil-game)

(defclass killer (person)
  ((last-kill-time :initform nil :accessor last-kill-time)))

;(defmethod act ((this killer))
;  (get-neighbors sil-game this)

(defmethod tick :before ((game sil-game) (killer killer))
  (when (and
         (< (random 100) 5)
         (kill-cooldown-ok killer))
    (let ((persons (get-near-persons-not-of-type game killer 'police)))
      (when persons
        (setf (last-kill-time killer) (real-time-seconds))
        (do-kill game killer (first persons))))))

(defmethod kill-cooldown-ok ((this killer))
  (or
   (null (last-kill-time this))
   (> (- (real-time-seconds) (last-kill-time this)) 1)))

(defmethod render ((this killer))
  (render-avatar this :killer))
