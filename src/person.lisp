(cl:in-package :sil-game)

(defclass person ()
  ((destination :initform nil :accessor destination)
   (path :initform nil :accessor path)
   (target :initform nil :accessor target)
   (state :initform 'wander :accessor state)
   (rest-time :initform (+ 0.5 (/ 1 (+ 0.1 (random 10)))) :accessor rest-time)
   (row :initform nil :accessor row)
   (col :initform nil :accessor col)
   (health :initform 100 :accessor health)
   (sick :initform nil :accessor sick)
   (gender :initform (nth (random 2) (list 'male 'female)) :accessor gender)
   (last-cough-time :initform nil :accessor last-cough-time)
   (last-move-time :initform (/ 1 (1+ (random 10))) :accessor last-move-time)))

(defmethod state-p ((this person) state)
  (equal (state this) state))

(defmethod regular-person-p ((person person))
  (not (or (typep person 'police)
           (typep person 'killer)
           (typep person 'medic))))

(defmethod become-sick ((this person))
  (when (not (sick this))
    (play :cough)
    (setf (last-cough-time this) (real-time-seconds))
    (setf (sick this) T)))

(defmethod become-healthy ((this person))
  (setf (health this) 100)
  (setf (sick this) nil)
  (play :heal))

(defmethod cough ((this person))
  (decf (health this) *sick-cough-damage*)
  (setf (last-cough-time this) (real-time-seconds)))

(defmethod move-person ((game sil-game) person to-row to-col)
  (when (and (typep person 'killer) (locked person))
    (return-from move-person))

  (let ((from-row (row person))
        (from-col (col person))
        (cells (cells game)))

    (when (and (typep person 'killer)
               (in-quarantine-p game from-row from-col)
               (not (in-quarantine-p game to-row to-col)))
      (lose-disguise game person)
      (setf (last-kill-time person) (real-time-seconds)))
     
    (setf (aref cells from-row from-col) nil)
    (setf (aref cells to-row to-col) person)
    (setf (row person) to-row)
    (setf (col person) to-col)
    (setf (last-move-time person) (real-time-seconds))))

(defmethod tick ((game sil-game) (person person))
  (when (sick person)
    (when (> (- (real-time-seconds) (last-cough-time person)) (/ 1 *sick-cough-frequency*))
      (cough person)
      (when (<= (health person) 0)
        (remove-person game person)
        (play :death)
        (return-from tick)))
    (dolist (near-person (get-near-persons game person))
      (when (= 0 (random 500))
        (become-sick near-person))))
  (with-slots (rest-time last-move-time) person
    (when (and
           (state-p person 'wander)
           (> (- (real-time-seconds) last-move-time) rest-time))
        (let* ((to (get-random-move-cell game person))
               (to-row (first to))
               (to-col (second to)))
          (when to
            (setf (rest-time person) (/ (+ 1000 (random 2000)) 1000))
            (move-person game person to-row to-col))))))


(defmethod render-avatar ((this person) asset)
  (with-pushed-canvas ()
    (let* ((width (image-width asset))
           (height (image-height asset))
           (scale-for (max width height))
           (scale (/ (- *cell-size* *cell-padding*) scale-for))
           (scaled-cell-size (/ *cell-size* scale)))
      (when (sick this)
        (let ((alpha (+ 0.5 (- 0.5 (/ (health this) 100)))))
          (draw-rect (vec2 0 0) *cell-size* *cell-size*
                     :fill-paint (vec4 0.9 0.1 0.1 alpha))))

      (scale-canvas scale scale)
      (draw-image
       (vec2 (- (/ scaled-cell-size 2) (/ width 2))
             (- (/ scaled-cell-size 2) (/ height 2)))
       asset))))

(defmethod render ((person person))
  (if (equal (gender person) 'female)
      (render-avatar person :person-female)
      (render-avatar person :person-male)))
