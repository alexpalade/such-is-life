(cl:in-package :sil-game)

(defparameter *population-percentage* 15)
(defparameter *sick-percentage* 10)
(defparameter *medics-count* 1)
(defparameter *police-count* 1)
(defparameter *killers-count* 3)
(defparameter *sick-cough-frequency* 5)
(defparameter *sick-cough-damage* 1)

(defparameter *rows-adjusted* nil)
(defparameter *population-percentage-adjusted* nil)
(defparameter *sick-percentage-adjusted* nil)
(defparameter *medics-count-adjusted* nil)
(defparameter *police-count-adjusted* nil)
(defparameter *killers-count-adjusted* nil)
(defparameter *sick-cough-frequency-adjusted* nil)
(defparameter *sick-cough-damage-adjusted* nil)

(defmethod restart-game ((game sil-game))

  (setf (state game) 'in-progress)

  (when *rows-adjusted*
    (setf *rows* *rows-adjusted*))

  (when *population-percentage-adjusted*
    (setf *population-percentage* *population-percentage-adjusted*))

  (when *sick-percentage-adjusted*
    (setf *sick-percentage* *sick-percentage-adjusted*))

  (when *medics-count-adjusted*
    (setf *medics-count* *medics-count-adjusted*))

  (when *police-count-adjusted*
    (setf *police-count* *police-count-adjusted*))

  (when *killers-count-adjusted*
    (setf *killers-count* *killers-count-adjusted*))

  (when *sick-cough-damage-adjusted*
    (setf *sick-cough-damage* *sick-cough-damage-adjusted*))

  (when *sick-cough-frequency-adjusted*
    (setf *sick-cough-frequency* *sick-cough-frequency-adjusted*))

  ;; recalculating stuff. yuck!
  (setf *cell-size* (/ *grid-height* *rows*))
  (setf *cell-half* (/ *cell-size* 2))
  (setf *cols* (floor (/ (- *stage-width* (* 2 *padding-bottom*)) *cell-size*)))
  (setf *padding-left* (/ (- *stage-width* (* *cell-size* *cols*)) 2))
  (setf *grid-width* (- *stage-width* (* 2 *padding-left*)))
  (setf *cols* (floor (/ *grid-width* *cell-size*)))
  (setf *cell-padding* (* 0.1 *cell-size*))

  (setf *grid-thickness* (- 1 (/ *rows* *grid-thickness-to-rows-factor*)))

  (let ((r (x *grid-base-color*))
        (g (y *grid-base-color*))
        (b (z *grid-base-color*))
        (a (alexandria:clamp (/ 0.2 *grid-thickness*) 0 1)))
    (setf *grid-color* (vec4 r g b a)))

  (with-slots (grid cells persons quarantine-from quarantine-to start-time statistics) game
    (setf grid (make-instance 'grid :rows *rows* :cols *cols* :cell-size *cell-size*))
    (setf cells (make-array (list *rows* *cols*) :initial-element nil))
    (setf persons nil)
    (setf quarantine-from nil)
    (setf quarantine-to nil)

    (setf start-time (real-time-seconds))

    (setf (hospital game) nil)
    (let ((random-cell (random-empty-cell game)))
      (place-hospital game (first random-cell) (second random-cell)))

    (let* ((population-percentage (/ *population-percentage* 100))
           (num-cells (* *rows* *cols*))
           (how-many-persons (round (* population-percentage num-cells)))
           (sick-percentage (/ *sick-percentage* 100))
           (how-many-sick (round (* sick-percentage how-many-persons))))
      (spawn-persons game 'person how-many-persons)
      (setf (getf statistics :alive) how-many-persons)
      (setf (getf statistics :dead) 0)
      (setf (getf statistics :sick) how-many-sick)

      ;; have at least one sick person
      (when (plusp sick-percentage)
        (setf how-many-sick (max 1 how-many-sick)))

      (dotimes (n how-many-sick)
        (let ((person (nth n (persons game))))
          (become-sick person)
          ;; randomize a bit, so they don't all die at once
          (setf (last-cough-time person)
                (+ (real-time-seconds) (/ (random 2000) 1000))))))

    (spawn-persons game 'police *police-count*)

    (spawn-persons game 'killer *killers-count*)

    (spawn-persons game 'medic *medics-count*)))

(defmethod update-label-alive ((game sil-game) (label label))
  (setf (text label)
        (concatenate 'string
                      "Alive: "
                      (write-to-string
                       (getf (statistics game) :alive)))))

(defmethod update-label-dead ((game sil-game) (label label))
  (setf (text label)
        (concatenate 'string
                      "Dead: "
                      (write-to-string
                       (getf (statistics game) :dead)))))

(defmethod update-label-sick ((game sil-game) (label label))
  (let ((sick (getf (statistics game) :sick)))
    (if (plusp sick)
        (setf (status-image label) :not-ok-sign)
        (setf (status-image label) :ok-sign))
    (setf (text label)
          (concatenate 'string
                       "Sick: "
                       (write-to-string sick)))))

(defmethod update-label-killers ((game sil-game) (label label))
  (let ((killers (getf (statistics game) :killers)))
    (if (plusp killers)
        (setf (status-image label) :not-ok-sign)
        (setf (status-image label) :ok-sign))

    (setf (text label)
          (concatenate 'string
                       "Killers: "
                       (write-to-string killers)))))

(defmethod update-status-bar ((game sil-game) (bar status-bar))
  (setf (alive bar) (getf (statistics game) :alive))
  (setf (sick bar) (getf (statistics game) :sick))
  (setf (dead bar) (getf (statistics game) :dead)))

(defmethod post-initialize ((game sil-game))
  (restart-game game)

  ;; UI elements
  (let* ((panel (panel game))
         (title (make-instance 'label :text "Such Is Life")
                                         :no-padding t)
         (subtitle (make-instance 'label :text "Lisp Game Jam"
                                         :no-padding t))
         (alive (make-instance 'label :text "Alive: ?"
                                      :text-align 'left
                                      :update #'update-label-alive))
         (dead (make-instance 'label :text "Dead: ?"
                                     :no-padding t
                                     :text-align 'left
                                     :update #'update-label-dead))
         (sick (make-instance 'label :text "Sick: ?"
                                     :no-padding t
                                     :text-align 'left
                                     :update #'update-label-sick))
         (killers (make-instance 'label :text "Killers: ?"
                                     :no-padding t
                                     :text-align 'left
                                     :update #'update-label-killers))
         (status-bar (make-instance 'status-bar
                                     :no-padding t
                                     :update #'update-status-bar))
         (restart-button (make-instance 'button :text "Restart"
                                                :action(lambda () (restart-game game))))
         (size-adjuster (make-instance 'adjuster
                                       :allowed-values '(10 15 20 25 30 40 50)
                                       :current-value *rows*
                                       :text "Size"
                                       :action (lambda (value) (setf *rows-adjusted* value))))
         (population-adjuster (make-instance 'adjuster
                                             :allowed-values '(1 2 3 4 5 7 10 15 20 30 40)
                                             :current-value *population-percentage*
                                             :text "People %"
                                             :action (lambda (value) (setf *population-percentage-adjusted* value))))
         (sick-adjuster (make-instance 'adjuster
                                       :allowed-values '(0 1 5 10 15 20 30 40 50 60 70 80 90 100)
                                       :current-value *sick-percentage*
                                       :text "Sick %"
                                       :action (lambda (value) (setf *sick-percentage-adjusted* value))))
         (police-adjuster (make-instance 'adjuster
                                         :allowed-values '(0 1 2 3 4 5 6 7 8 9 10)
                                         :current-value *police-count*
                                         :text "Police"
                                         :action (lambda (value) (setf *police-count-adjusted* value))))
         (medics-adjuster (make-instance 'adjuster
                                         :allowed-values '(0 1 2 3 4 5 6 7 8 9 10)
                                         :current-value *medics-count*
                                         :text "Medics"
                                         :action (lambda (value) (setf *medics-count* value))))
         (killers-adjuster (make-instance 'adjuster
                                          :allowed-values '(0 1 2 3 4 5 6 7 8 9 10)
                                          :current-value *killers-count*
                                          :text "Killers"
                                          :action (lambda (value) (setf *killers-count-adjusted* value))))
         (cough-frequency-adjuster (make-instance 'adjuster
                                         :allowed-values '(0.5 1 2 3 4 5 6 7 8 9 10)
                                         :current-value *sick-cough-frequency*
                                         :text "Rate"
                                         :action (lambda (value) (setf *sick-cough-frequency-adjusted* value))))
         (cough-damage-adjuster (make-instance 'adjuster
                                         :allowed-values '(0.1 0.5 1 2 5 10)
                                         :current-value *sick-cough-damage*
                                         :text "Damage"
                                         :action (lambda (value) (setf *sick-cough-damage-adjusted* value)))))
    (add-element panel title)
    (add-element panel subtitle)
    (add-element panel (make-instance 'separator))
    (add-element panel (make-instance 'label :text "Statistics"))
    (add-element panel alive)
    (add-element panel dead)
    (add-element panel sick)
    (add-element panel killers)
    (add-element panel status-bar)
    (add-element panel (make-instance 'separator))
    (add-element panel (make-instance 'label :text "Settings"))
    (add-element panel size-adjuster)
    (add-element panel population-adjuster)
    (add-element panel sick-adjuster)
    (add-element panel medics-adjuster)
    (add-element panel police-adjuster)
    (add-element panel killers-adjuster)
    (add-element panel (make-instance 'separator))
    (add-element panel (make-instance 'label :text "Virus..."))
    (add-element panel cough-frequency-adjuster)
    (add-element panel cough-damage-adjuster)
    (add-element panel (make-instance 'separator))
    (add-element panel restart-button))

  (bind-button :escape :pressed #'gamekit:stop)
  (bind-button :q :pressed #'gamekit:stop)

  (bind-cursor (lambda (x y)
                 "Save cursor position"
                 (setf (gamekit:x *cursor-pos*) x
                       (gamekit:y *cursor-pos*) y)
                 (handle-cursor-move game)))

  (bind-button :mouse-right :pressed
               (lambda ()
                 (let ((row (first (cursor-to-cell)))
                       (col (second (cursor-to-cell))))
                   (handle-right-click-cell game row col))))

  (bind-button :mouse-left :released
               (lambda ()
                 (setf *area-dragging-p* nil)))

  (bind-button :mouse-left :pressed
               (lambda ()
                 (let ((row (first (cursor-to-cell)))
                       (col (second (cursor-to-cell))))
                   (handle-click-cell game row col)
                   (click-event (panel game) *cursor-pos*)))))

(defun cursor-to-cell ()
  (let* ((x-mouse (x *cursor-pos*))
         (y-mouse (y *cursor-pos*))
         (col (floor (/ (- x-mouse *padding-left*) *cell-size*)))
         (row (floor (/ (- y-mouse *padding-bottom*) *cell-size*))))
    (list row col)))

(defmethod handle-cursor-move ((this sil-game))
  (when (and
         *area-dragging-p*
         (cell-valid-p this (first (cursor-to-cell)) (second (cursor-to-cell))))
    (setf (quarantine-to this) (cursor-to-cell))))

(defmethod handle-click-cell ((this sil-game) row col)
  (when (cell-valid-p this row col)
    (let ((obj (aref (cells this) row col)))
      (format t "Cell: ~A x ~A ~%" row col)
      (when (not (cell-free-p this row col))
        (when (person-p obj)
          (format t "State: ~A~%" (state (aref (cells this) row col)))))
        (setf *area-dragging-p* t)
        (setf (quarantine-to this) (cursor-to-cell))
        (setf (quarantine-from this) (cursor-to-cell)))))

(defmethod handle-right-click-cell ((this sil-game) row col)
  (when (and (cell-valid-p this row col)
             (cell-free-p this row col))
    (place-hospital this row col)))

(defmethod get-empty-cells ((this sil-game))
  (let ((result '()))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (null (aref (cells this) row col))
          (push (list row col) result))))
    result))

(defmethod random-empty-cell ((this sil-game))
  (random-nth (get-empty-cells this)))

(defmethod spawn-persons ((this sil-game) type &optional (how-many 1))
  (dotimes (n how-many)
    (spawn-person this type)))

(defmethod spawn-person ((this sil-game) type)
  (let ((person (make-instance type))
        (free-pos (random-empty-cell this)))
    (when (null free-pos)
      (format t "Warning: couldn't spawn person on null position: ~A ~%" free-pos)
      (return-from spawn-person))
    (push person (persons this))
    (setf (row person) (first free-pos))
    (setf (col person) (second free-pos))
    (setf (aref (cells this) (first free-pos) (second free-pos)) person)))

(defun cell-pos (row col)
  (vec2 (* col *cell-size*) (* row *cell-size*)))

(defun translate-canvas-vec (vec)
  (translate-canvas (x vec) (y vec)))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defmethod get-closest-sick-person ((game sil-game) from-row from-col)
  (let* ((closest-distance (* *rows* *cols*))
         (closest-sick nil))
    (dolist (p (persons game))
      (when (and (not (and (typep p 'killer) (locked p)))
                 (sick p)
                 (state-p p 'wander))
        (let ((distance (distance-between from-row from-col (row p) (col p))))
          (when (< distance closest-distance)
            (setf closest-distance distance)
            (setf closest-sick p)))))
    closest-sick))

(defmethod get-closest-killer ((game sil-game) from-row from-col)
  (let* ((closest-distance (* *rows* *cols*))
         (closest-killer nil))
    (dolist (k (get-all-persons-of-type game 'killer))
      (when (and
             (not (disguised k))
             (not (locked k)))
       (let ((distance (distance-between from-row from-col (row k) (col k))))
          (when (< distance closest-distance)
            (setf closest-distance distance)
            (setf closest-killer k)))))
    closest-killer))

(defmethod quarantine-corners ((this sil-game))
  (when (and (quarantine-from this)
             (quarantine-to this))
    (let* ((from-row (first (quarantine-from this)))
           (from-col (second (quarantine-from this)))
           (to-row (first (quarantine-to this)))
           (to-col (second (quarantine-to this)))
           (top-row (max from-row to-row))
           (bottom-row (min from-row to-row))
           (left-col (min from-col to-col))
           (right-col (max from-col to-col)))
      (list :left-col left-col
            :right-col right-col
            :top-row top-row
            :bottom-row bottom-row))))

(defmethod in-quarantine-person-p ((this sil-game) (person person))
  (in-quarantine-p this (row person) (col person)))

(defmethod in-quarantine-p ((this sil-game) row col)
  (when (and (quarantine-from this)
             (quarantine-to this))
    (let* ((corners (quarantine-corners this))
           (top-row (getf corners :top-row))
           (bottom-row (getf corners :bottom-row))
           (left-col (getf corners :left-col))
           (right-col (getf corners :right-col)))
      (and (<= row top-row)
           (>= row bottom-row)
           (>= col left-col)
           (<= col right-col)))))

(defmethod reached-target-p ((game sil-game) (person person))
  (find (target person) (get-near-persons game person)))

(defmethod render-path ((game sil-game) (p person))
  (when (null (path p))
    (return-from render-path))
  (dolist (node (path p))
    (let* ((row (first node))
           (col (second node))
           (x (+ (* col *cell-size*) *cell-half*))
           (y (+ (* row *cell-size*) *cell-half*)))
      (draw-circle (vec2 x y)
                   (/ *cell-size* 30)
                   :fill-paint *path-color*))))

(defmethod update-path-person ((game sil-game) (p person))
  (setf
   (path p)
   (find-path game
              (row p)
              (col p)
              (first (destination p))
              (second (destination p)))))

(defun cells-adjacent-p (row1 col1 row2 col2)
  (<= (distance-between row1 col1 row2 col2) 1))

(defmethod get-cell ((this sil-game) row col)
  (when (cell-valid-p this row col)
    (aref (cells this) row col)))

;; retuns A LIST of elements like '(row col)
(defmethod get-near-cells-person ((person person))
  (get-near-cells (row person) (col person) *rows* *cols*))

;; retuns a list of persons
(defmethod get-near-persons ((game sil-game) (person person))
  (let ((near-cells (get-near-cells-person person))
        (cells (cells game)))
    (remove-if
     (lambda (cell) (or (null cell) (not (person-p cell))))
     (map 'list
          (lambda (cell) (aref cells (first cell) (second cell)))
          near-cells))))

(defun person-p (o)
  (typep o 'person))

(defmethod get-near-persons-of-type ((game sil-game) (person person) type)
  (remove-if-not
   (lambda (p) (typep p type))
   (get-near-persons game person)))

(defmethod get-near-persons-not-of-type ((game sil-game) (person person) type)
  (remove-if
   (lambda (p) (typep p type))
   (get-near-persons game person)))

(defmethod get-near-sick-persons ((game sil-game) (person person))
  (remove-if-not
   (lambda (p) (sick p))
   (get-near-persons game person)))

(defmethod cell-valid-p ((this sil-game) row col)
  (and (>= row 0) (>= col 0) (< row *rows*) (< col *cols*)))

(defmethod cell-free-p ((this sil-game) row col)
  (cell-free-ignore-p this row col nil))

(defmethod cell-free-ignore-p ((this sil-game) row col ignore-type)
  (let ((cell (aref (cells this) row col)))
    (if (or (null cell)
            (and ignore-type (equal (type-of cell) ignore-type)))
        T
        nil)))

(defmethod get-near-free-cells ((this sil-game) row col)
  (get-near-free-cells-ignore this row col nil))

(defmethod get-near-free-cells-ignore ((this sil-game) row col ignore-type)
  (let* ((result-cells '())
         (near-cells (get-near-cells row col *rows* *cols*)))
    (dolist (try-cell near-cells)
      (when (and
             (cell-valid-p this (first try-cell) (second try-cell))
             (cell-free-ignore-p this (first try-cell) (second try-cell) ignore-type))
        (push try-cell result-cells)))
    result-cells))

(defmethod get-near-free-cells-person ((this sil-game) person)
  (get-near-free-cells this (row person) (col person)))

(defmethod get-random-move-cell ((this sil-game) person)
  (let* ((cells (get-near-free-cells-person this person)))
    (when (and (in-quarantine-person-p this person)
               (regular-person-p person))
      (setf cells (remove-if-not
                   (lambda (cell)
                     (in-quarantine-p this (first cell) (second cell)))
                   cells)))
    (if cells
        (random-nth cells)
        nil)))

(defmethod get-all-persons-of-type ((this sil-game) type)
  (remove-if-not
   (lambda (p) (typep p type))
   (persons this)))

(defmethod get-killers ((this sil-game))
  (get-all-persons-of-type this 'killer))

(defmethod get-medics ((this sil-game))
  (get-all-persons-of-type this 'medic))

(defmethod remove-person ((game sil-game) person)
  (when (regular-person-p person)
    (incf (getf (statistics game) :dead)))
  (setf (aref (cells game) (row person) (col person)) nil)
  (setf (persons game) (remove person (persons game))))

(defmethod do-kill ((this sil-game) killer person)
  ;; medic died, released grabbed person
  (when (and (typep person 'medic)
             (state-p person 'grab-sick)
             (not (null (target person))))
    (setf (state (target person)) 'wander))

  (remove-person this person)
  (move-person this killer (row person) (col person))
  (play :kill))

(defclass hospital ()
  ((row :initarg row :accessor row)
   (col :initarg col :accessor col)))

(defmethod render ((this hospital))
  (with-pushed-canvas ()
    (let* ((asset :hospital)
           (width (image-width asset))
           (height (image-height asset))
           (scale-for (max width height))
           (scale (/ (- *cell-size* *cell-padding*) scale-for))
           (scaled-cell-size (/ *cell-size* scale)))
      (scale-canvas scale scale)
      (draw-image
       (vec2 (- (/ scaled-cell-size 2) (/ width 2))
             (- (/ scaled-cell-size 2) (/ height 2)))
       asset))))

(defmethod place-hospital ((this sil-game) row col)
  (if (null (hospital this))
      (setf (hospital this) (make-instance 'hospital))
      (setf (aref (cells this) (row (hospital this)) (col (hospital this))) nil))
  (setf (row (hospital this)) row)
  (setf (col (hospital this)) col)
  (setf (aref (cells this) row col) (hospital this)))

(defmethod gamekit:draw ((game sil-game))

  ;; background color
  (draw-rect (vec2 0 0)
             *width*
             *height*
             :fill-paint *background-color*)

  (draw-rect (vec2 *padding-left* *padding-bottom*)
             (- *stage-width* (* *padding-left* 2))
             (- *stage-height* (* *padding-bottom* 2))
             :stroke-paint *grid-border-color*
             :fill-paint nil
             :thickness *grid-border-thickness*)

  (render (panel game))

  (with-pushed-canvas ()
    (translate-canvas *padding-left* *padding-bottom*)
    (render (grid game))
    (dolist (medic (get-medics game))
      (when (not (state-p medic 'wander))
        (render-path game medic)))
    (render game)

    ;; draw quarantine
    (when (and (quarantine-to game) (quarantine-from game))
      (let* ((corners (quarantine-corners game))
             (top-row (getf corners :top-row))
             (bottom-row (getf corners :bottom-row))
             (left-col (getf corners :left-col))
             (right-col (getf corners :right-col))
             (from-x (* left-col *cell-size*))
             (from-y (* bottom-row *cell-size*))
             (to-x (+ (* right-col *cell-size*) *cell-size*))
             (to-y (+ (* top-row *cell-size*) *cell-size*))
             (width (abs (- from-x to-x)))
             (height (abs (- from-y to-y))))
        (draw-rect (vec2 from-x from-y) width height
                 :thickness *quarantine-border-thickness*
                 :stroke-paint *quarantine-border-color*)))))

(defmethod render ((game sil-game))
  (with-slots (cells) game
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (aref cells row col)
          (with-pushed-canvas ()
            (translate-canvas-vec (cell-pos row col))
            (render (aref cells row col))))))))

(defmethod update-statistics ((game sil-game))
  (let ((persons (persons game))
        (alive 0)
        (killers 0)
        (sick 0))
    (dolist (person persons)
      (when (and (typep person 'killer) (not (locked person)))
        (incf killers))
      (when (regular-person-p person)
        (incf alive)
        (when (sick person)
          (incf sick))))
    (setf (getf (statistics game) :alive) alive)
    (setf (getf (statistics game) :sick) sick)
    (setf (getf (statistics game) :killers) killers)))

(defmethod check-win-condition ((game sil-game))
  (let ((sick (getf (statistics game) :sick))
        (killers (getf (statistics game) :killers)))
    (when (and
           (equal (state game) 'in-progress)
           (zerop sick)
           (zerop killers))
      (setf (state game) 'game-over)
      (play :win))))

(defmethod act ((game sil-game))
  (dolist (person (persons game))
    (tick game person))
  (update-statistics game)
  (check-win-condition game)
  (panel-act game))

(defmethod run ()
  (gamekit:start 'sil-game))
