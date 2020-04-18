(cl:in-package :sih)

(defvar *width* 800)
(defvar *height* 600)

;; resouces
(register-resource-package :keyword (asdf:system-relative-pathname :society-is-hard "assets/"))
(define-image :person "person.png")
(define-image :police "police.png")
(define-image :killer "killer.png")
(define-image :medic "medic.png")

(define-image :hospital "hospital.png")

(define-image :tile "tile.png")
(define-image :border-tile "border-tile.png")

(define-sound :grab "grab.ogg")
(define-sound :death "death.ogg")

(defvar *cursor-pos* (gamekit:vec2 0 0))

(defparameter *padding-bottom* 20)
(defparameter *grid-height* (- *height* (* 2 *padding-bottom*)))
(defparameter *rows* 20)
(defparameter *cell-size* (/ *grid-height* *rows*))
(defparameter *cell-half* (/ *cell-size* 2))
(defparameter *cols* (floor (/ (- *width* (* 2 *padding-bottom*)) *cell-size*)))
(defparameter *padding-left* (/ (- *width* (* *cell-size* *cols*)) 2))
(defparameter *grid-width* (- *width* (* 2 *padding-left*)))
(defparameter *cols* (floor (/ *grid-width* *cell-size*)))
(defparameter *once* nil)

(defparameter *area-dragging-p* nil)

;; % padding between a cell and the image assets
(defparameter *cell-padding* (* 0.1 *cell-size*))

(defvar *black* (gamekit:vec4 0 0 0 1))

(gamekit:defgame sih ()
  ((grid :initform (make-instance 'grid :rows *rows* :cols *cols* :cell-size *cell-size*) :accessor grid)
   (persons :initform '() :accessor persons)
   (cells :accessor cells)
   (hospital :accessor hospital :initform nil)
   (quarantine-from :accessor quarantine-from :initform nil)
   (quarantine-to :accessor quarantine-to :initform nil))

  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Society Is Hard Game"))

(defmethod post-initialize ((this sih))
  (setf (cells this) (make-array (list *rows* *cols*) :initial-element nil))

  (place-hospital this 0 0)

  (spawn-persons this 'person 100)

  (become-sick (first (persons this)))
  (become-sick (second (persons this)))
  (become-sick (third (persons this)))
  (become-sick (fourth (persons this)))

  (spawn-persons this 'killer 2)
  (spawn-persons this 'police 2)
  (spawn-persons this 'medic 3)

  (bind-button :escape :pressed #'gamekit:stop)
  (bind-button :q :pressed #'gamekit:stop)

  (bind-cursor (lambda (x y)
                 "Save cursor position"
                 (setf (gamekit:x *cursor-pos*) x
                       (gamekit:y *cursor-pos*) y)
                 (handle-cursor-move this)))

  (bind-button :mouse-left :released
               (lambda ()
                 (setf *once* nil)
                 (setf *area-dragging-p* nil)))

  (bind-button :mouse-left :pressed
               (lambda ()
                 (let ((row (first (cursor-to-cell)))
                       (col (second (cursor-to-cell))))
                   (handle-click-cell this row col)))))

(defun cursor-to-cell ()
  (let* ((x-mouse (x *cursor-pos*))
         (y-mouse (y *cursor-pos*))
         (col (floor (/ (- x-mouse *padding-left*) *cell-size*)))
         (row (floor (/ (- y-mouse *padding-bottom*) *cell-size*))))
    (list row col)))

(defmethod handle-cursor-move ((this sih))
  (when *area-dragging-p*
    (setf (quarantine-to this) (cursor-to-cell))))

(defmethod handle-click-cell ((this sih) row col)
  (setf *once* nil)
  (when (cell-valid-p this row col)
    (let ((obj (aref (cells this) row col)))
      (format t "Cell: ~A x ~A ~%" row col)
      (when (not (cell-free-p this row col))
        (when (person-p obj)
          (format t "State: ~A~%" (state (aref (cells this) row col))))
        (when (typep obj 'medic)
          (format t "Path: ~A~%" (path obj))))
      (when (or (cell-free-p this row col)
                (not (person-p obj)))
        (setf *area-dragging-p* t)
        (setf (quarantine-to this) (cursor-to-cell))
        (setf (quarantine-from this) (cursor-to-cell))))))

    ;(when (cell-free-p this row col)
    ;  (place-hospital this row col)
    ;  (let ((med (first (get-medics this))))
    ;    (when med
    ;      (setf (destination med) (list row col))
    ;      (update-path-person this med)
    ;      (setf (state med) 'target)
    ;      (format t "~A~%" (path med)))))))

                                        ;(when (not (cell-free-p this row col))
                                        ;  (format t "~A~%" (get-near-persons this (get-cell this row col)))
                                        ;  (play :grab))

(defmethod get-empty-cells ((this sih))
  (let ((result '()))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (null (aref (cells this) row col))
          (push (list row col) result))))
    result))

(defmethod random-empty-cell ((this sih))
  (random-nth (get-empty-cells this)))

(defmethod spawn-persons ((this sih) type &optional (how-many 1))
  (dotimes (n how-many)
    (spawn-person this type)))

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
           (scaled-cell-size (/ *cell-size* scale))
           (scale-no-padding (/ *cell-size* scale-for))
           (scaled-cell-size-no-padding (/ *cell-size* scale-no-padding)))

      (scale-canvas scale scale)
      (draw-image
       (vec2 (- (/ scaled-cell-size 2) (/ width 2))
             (- (/ scaled-cell-size 2) (/ height 2)))
       asset))))

(defmethod place-hospital ((this sih) row col)
  (if (null (hospital this))
      (setf (hospital this) (make-instance 'hospital))
      (setf (aref (cells this) (row (hospital this)) (col (hospital this))) nil))
  (setf (row (hospital this)) row)
  (setf (col (hospital this)) col)
  (setf (aref (cells this) row col) (hospital this)))

(defmethod spawn-person ((this sih) type)
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

(defun draw-time ()
  (draw-text (write-to-string (coerce (real-time-seconds) 'float)) (vec2 30 500)))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defmethod get-sick-person ((this sih))
  (dolist (p (alexandria:shuffle (persons this)))
    (when (and (sick p)
               (state-p p 'wander))
      (return-from get-sick-person p))))

(defmethod quarantine-corners ((this sih))
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

(defmethod in-quarantine-person-p ((this sih) (person person))
  (in-quarantine-p this (row person) (col person)))

(defmethod in-quarantine-p ((this sih) row col)
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

(defmethod reached-target-p ((game sih) (person person))
  (find (target person) (get-near-persons game person)))

(defmethod render-path ((game sih) (p person))
  (when (null (path p))
    (return-from render-path))
  (dolist (node (path p))
    (let* ((row (first node))
           (col (second node))
           (x (+ (* col *cell-size*) *cell-half*))
           (y (+ (* row *cell-size*) *cell-half*)))
      (draw-circle (vec2 x y)
                   (/ *cell-size* 15)
                   :fill-paint *black*))))

(defmethod update-path-person ((game sih) (p person))
  (setf
   (path p)
   (find-path game
              (row p)
              (col p)
              (first (destination p))
              (second (destination p)))))

(defun cells-adjacent-p (row1 col1 row2 col2)
  (<= (distance-between row1 col1 row2 col2) 1))

(defmethod get-cell ((this sih) row col)
  (when (cell-valid-p this row col)
    (aref (cells this) row col)))

;; retuns A LIST of elements like '(row col)
(defmethod get-near-cells-person ((person person))
  (get-near-cells (row person) (col person) *rows* *cols*))

;; retuns a list of persons
(defmethod get-near-persons ((game sih) (person person))
  (let ((near-cells (get-near-cells-person person))
        (cells (cells game)))
    (remove-if
     (lambda (cell) (or (null cell) (not (person-p cell))))
     (map 'list
          (lambda (cell) (aref cells (first cell) (second cell)))
          near-cells))))

(defun person-p (o)
  (typep o 'person))

(defmethod get-near-persons-of-type ((game sih) (person person) type)
  (remove-if-not
   (lambda (p) (typep p type))
   (get-near-persons game person)))

(defmethod get-near-persons-not-of-type ((game sih) (person person) type)
  (remove-if
   (lambda (p) (typep p type))
   (get-near-persons game person)))

(defmethod get-near-sick-persons ((game sih) (person person))
  (remove-if-not
   (lambda (p) (sick p))
   (get-near-persons game person)))

(defmethod cell-valid-p ((this sih) row col)
  (and (>= row 0) (>= col 0) (< row *rows*) (< col *cols*)))

(defmethod cell-free-p ((this sih) row col)
  (cell-free-ignore-p this row col nil))

(defmethod cell-free-ignore-p ((this sih) row col ignore-type)
  (let ((cell (aref (cells this) row col)))
    (if (or (null cell)
            (and ignore-type (equal (type-of cell) ignore-type)))
        T
        nil)))

(defmethod get-near-free-cells ((this sih) row col)
  (get-near-free-cells-ignore this row col nil))

(defmethod get-near-free-cells-ignore ((this sih) row col ignore-type)
  (let* ((result-cells '())
         (near-cells (get-near-cells row col *rows* *cols*)))
    (dolist (try-cell near-cells)
      (when (and
             (cell-valid-p this (first try-cell) (second try-cell))
             (cell-free-ignore-p this (first try-cell) (second try-cell) ignore-type))
        (push try-cell result-cells)))
    result-cells))

(defmethod get-near-free-cells-person ((this sih) person)
  (get-near-free-cells this (row person) (col person)))

(defmethod get-random-move-cell ((this sih) person)
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

(defmethod get-all-persons-of-type ((this sih) type)
  (remove-if-not
   (lambda (p) (typep p type))
   (persons this)))

(defmethod get-killers ((this sih))
  (get-all-persons-of-type this 'killer))

(defmethod get-medics ((this sih))
  (get-all-persons-of-type this 'medic))

(defmethod remove-person ((this sih) person)
  (setf (aref (cells this) (row person) (col person)) nil)
  (setf (persons this) (remove person (persons this))))

(defmethod do-kill ((this sih) killer person)
  (remove-person this person)
  (move-person this killer (row person) (col person))
  (play :death))

(defmethod gamekit:draw ((this sih))
  (with-pushed-canvas ()
    (translate-canvas *padding-left* *padding-bottom*)
    (render (grid this))
    (dolist (medic (get-medics this))
      (render-path this medic))
    (render this)

    (when (and (quarantine-to this) (quarantine-from this))
      (let* ((corners (quarantine-corners this))
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
        (when (not *once*)
          (format t "cell size ~A ~%" *cell-size*)
          (format t "quarantine-from ~A ~%" (quarantine-from this))
          (format t "quarantine-to ~A ~%" (quarantine-to this))
          (format t "corners ~A ~%" corners)
          (format t "coords: ~A ~A -> ~A ~A ~%" from-x from-y to-x to-y)
          (setf *once* T))
        (draw-rect (vec2 from-x from-y) width height
                 :thickness 3
                 :stroke-paint (vec3 0.9 0.1 0.1))))))

(defmethod render ((this sih))
  (draw-time)
  (with-slots (cells) this
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (aref cells row col)
          (with-pushed-canvas ()
            (translate-canvas-vec (cell-pos row col))
            (render (aref cells row col))))))))

(defmethod act ((game sih))
  (dolist (person (persons game))
    (tick game person)))

(defmethod run ()
  (gamekit:start 'sih))
