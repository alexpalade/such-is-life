(cl:in-package :sil-game)

(defclass killer (person)
  ((disguised :initform t :accessor disguised)
   (gender :initform (nth (random 2) (list 'male 'female)) :accessor gender)
   (locked :initform nil :accessor locked)
   (last-kill-time :initform nil :accessor last-kill-time)))

(defmethod tick :before ((game sil-game) (killer killer))
  (when (locked killer)
    (return-from tick))
  ;; can disguise again after some time
  (when (and
         (not (disguised killer))
         (> (time-since-last-kill killer) 1))
    (setf (disguised killer) t))
  (when (and
         (< (random 100) 2)
         (kill-cooldown-ok killer))
    (let ((persons (get-near-persons-not-of-type game killer 'police)))
      ;; also remove other killers from the list
     (setf persons (remove-if (lambda (x) (typep x 'killer)) persons))
      (when persons
        (setf (last-kill-time killer) (real-time-seconds))
        (lose-disguise game killer)
        (do-kill game killer (first persons))))))

(defmethod lose-disguise ((game sil-game) (killer killer))
  (setf (disguised killer) nil))

(defmethod time-since-last-kill ((this killer))
   (- (real-time-seconds) (last-kill-time this)))

(defmethod kill-cooldown-ok ((this killer))
  (or
   (null (last-kill-time this))
   (> (time-since-last-kill this) 1)))

(defmethod render ((killer killer))
  (if (locked killer)
      (progn
        (render-avatar killer :killer)
        (render-avatar killer :prison))
      (if (disguised killer)
          (if (equal (gender killer) 'male)
              (render-avatar killer :killer-male-disguised)
              (render-avatar killer :killer-female-disguised))
          (if (equal (gender killer) 'male)
              (render-avatar killer :killer)
              (render-avatar killer :killer)))))
