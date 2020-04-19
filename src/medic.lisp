(cl:in-package :sil-game)

(defclass medic (person) ())

(defmethod render ((this medic))
  (render-avatar this :medic))

(defmethod tick ((game sil-game) (medic medic))
  ;; medic is idle, take a patient
  (when (state-p medic 'wander)
    (let ((sick-person (get-sick-person game)))
      (if (and
           sick-person
           (not (equal medic sick-person))
           (state-p sick-person 'wander))
          (progn
            (setf (target medic) sick-person)
            (setf (state medic) 'grab-sick))
          (call-next-method))))

  ;; medic is going for patient
  (when (and (state-p medic 'grab-sick)
             (> (- (real-time-seconds) (last-move-time medic)) (rest-time medic)))
    (with-slots (target path) medic
      (when (or (null target)
                (not (state-p target 'wander))
                (not (find target (persons game))))
        (setf (state medic) 'wander)
        (return-from tick))
      (when (or (null path)
                (not (equal (row target) (first (first path))))
                (not (equal (col target) (second (first path)))))
        (setf (destination medic) (list (row target) (col target)))
        (update-path-person game medic))
      (let* ((next-cell (first (path medic)))
             (to-row (first next-cell))
             (to-col (second next-cell)))
        (when (reached-target-p game medic)
          (setf (state medic) 'take-to-hospital)
          (setf (state (target medic)) 'grabbed)
          (setf (destination medic) (list
                                     (row (hospital game))
                                     (col (hospital game))))
          (update-path-person game medic)
          (return-from tick))
        (if (and to-row to-col (cell-free-p game to-row to-col))
            (progn
              (setf (rest-time medic) 0.3)
              (move-person game medic to-row to-col))
            (update-path-person game medic)))))

  ;; medic is taking someone to the hospital
  (when (and (state-p medic 'take-to-hospital)
             (> (- (real-time-seconds) (last-move-time medic)) (rest-time medic)))
    (let* ((next-cell (first (path medic)))
           (to-row (first next-cell))
           (to-col (second next-cell)))
      (when (not (find (target medic) (persons game)))
        (setf (state medic) 'wander)
        (return-from tick))
      (if (and (path medic)
               (cell-free-p game to-row to-col))
          (progn
            (setf (rest-time medic) 0.5)
            (let ((old-row (row medic))
                  (old-col (col medic)))
              (move-person game medic to-row to-col)
              (move-person game (target medic) old-row old-col)))
          (progn
            (setf (destination medic) (list (row (hospital game)) (col (hospital game))))
            (update-path-person game medic)))
      ;; yay, reached hospital
      (when (member (list (row (hospital game)) (col (hospital game)))
                    (get-near-cells-person medic)
                    :test (lambda (x y) (and (equal (first x) (first y)) (equal (second x) (second y)))))
        (become-healthy medic)
        (become-healthy (target medic))
        (setf (last-move-time medic) (real-time-seconds))
        (setf (last-move-time (target medic)) (real-time-seconds))
        (setf (state medic) 'wander)
        (setf (state (target medic)) 'wander)))))
