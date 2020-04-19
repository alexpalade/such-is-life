(cl:in-package :sil-game)

(defclass police (person)
   ((gender :initform (nth (random 2) (list 'male 'female)) :accessor gender)))

(defmethod tick :before ((game sil-game) (police police))
  ;; lock near killers
  (dolist (killer (get-near-persons-of-type game police 'killer))
    (when (and (not (disguised killer))
               (not (locked killer))))
      (setf (locked killer) t))

  ;; chase
  (when (and (state-p police 'chasing)
             (> (- (real-time-seconds) (last-move-time police)) (rest-time police)))
    (with-slots (target path) police
      (when (or (null target)
                (disguised target)
                (locked target)
                (not (find target (persons game))))
        (setf (state police) 'wander)
        (return-from tick))
      (when (or (null path)
                (not (equal (row target) (first (first path))))
                (not (equal (col target) (second (first path)))))
        (setf (destination police) (list (row target) (col target)))
        (update-path-person game police))
      (let* ((next-cell (first (path police)))
             (to-row (first next-cell))
             (to-col (second next-cell)))
        (if (and to-row to-col (cell-free-p game to-row to-col))
            (progn
              (setf (rest-time police) 0.3)
              (move-person game police to-row to-col))
            (update-path-person game police))))))

(defmethod render ((this police))
  (if (equal (gender this) 'female)
      (render-avatar this :police-female)
      (render-avatar this :police-male)))
