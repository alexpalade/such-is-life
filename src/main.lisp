(cl:in-package :sih)

(defvar *width* 800)
(defvar *height* 600)

;; resouces
(register-resource-package :keyword (asdf:system-relative-pathname :sih "assets/"))
(define-image :person "person.png")
(define-image :police "police.png")
(define-image :killer "killer.png")
(define-image :medic "medic.png")
(define-sound :grab "grab.ogg")

(defvar *cursor-pos* (gamekit:vec2 0 0))

(defparameter *padding-bottom* 20)
(defparameter *grid-height* (- *height* (* 2 *padding-bottom*)))
(defparameter *rows* 10)
(defparameter *cell-size* (/ *grid-height* *rows*))
(defparameter *cell-half* (/ *cell-size* 2))
(defparameter *cols* (floor (/ (- *width* (* 2 *padding-bottom*)) *cell-size*)))
(defparameter *padding-left* (/ (- *width* (* *cell-size* *cols*)) 2))
(defparameter *grid-width* (- *width* (* 2 *padding-left*)))
(defparameter *cols* (floor (/ *grid-width* *cell-size*)))
(defparameter *once* nil)

;; % padding between a cell and the image assets
(defparameter *cell-padding* (* 0.1 *cell-size*))

(defvar *black* (gamekit:vec4 0 0 0 1))

(gamekit:defgame sih ()
  ((grid :initform (make-instance 'grid :rows *rows* :cols *cols* :cell-size *cell-size*) :accessor grid)
   (persons :initform '() :accessor persons)
   (cells :accessor cells))

  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Society is Hard"))

(defmethod post-initialize ((this sih))
  (setf (cells this) (make-array (list *rows* *cols*) :initial-element nil))

  (dotimes (x 5)
    (spawn-person this 'person))

  (dotimes (x 2)
    (spawn-person this 'killer))

  (dotimes (x 2)
    (spawn-person this 'police))

  (dotimes (x 1)
    (spawn-person this 'medic))
 
  (bind-button :escape :pressed #'gamekit:stop)
  (bind-button :q :pressed #'gamekit:stop)

  (bind-cursor (lambda (x y)
                 "Save cursor position"
                 (setf (gamekit:x *cursor-pos*) x
                       (gamekit:y *cursor-pos*) y)))

  (bind-button :mouse-left :pressed
               (lambda ()
                 (let* ((x-mouse (x *cursor-pos*))
                        (y-mouse (y *cursor-pos*))
                        (col (floor (/ (- x-mouse *padding-left*) *cell-size*)))
                        (row (floor (/ (- y-mouse *padding-bottom*) *cell-size*))))
                   (handle-click-cell this row col)))))

(defmethod handle-click-cell ((this sih) row col)
  (when (and (is-cell-valid this row col)
             (not (is-cell-free this row col)))
    (play :grab)
    (format t "Clicked cell: ~A x ~A ~%" row col)))

(defmethod get-empty-cells ((this sih))
  (let ((result '()))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (null (aref (cells this) row col))
          (push (list row col) result))))
    result))

(defmethod random-empty-cell ((this sih))
  (random-nth (get-empty-cells this)))

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

(defmethod render ((this sih))
  (draw-time)
  (with-slots (cells) this
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (aref cells row col)
          (with-pushed-canvas ()
            (translate-canvas-vec (cell-pos row col))
            (render (aref cells row col))))))))

(defun draw-time ()
  (draw-text (write-to-string (coerce (real-time-seconds) 'float)) (vec2 30 500)))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defmethod is-cell-valid ((this sih) row col)
  (and (>= row 0) (>= col 0) (< row *rows*) (< col *cols*)))

(defmethod is-cell-free ((this sih) row col)
  (if (null (aref (cells this) row col))
      T
      nil))

(defmethod get-move-cells ((this sih) person)
  (let* ((result-cells '())
         (row (row person))
         (col (col person))
         (try-cells (list
                     (list (1- row) col)
                     (list (1+ row) col)
                     (list row (1+ col))
                     (list row (1- col)))))

    (dolist (try-cell try-cells)
      (when (and
             (is-cell-valid this (first try-cell) (second try-cell))
             (is-cell-free this (first try-cell) (second try-cell)))
        (push try-cell result-cells)))

    result-cells))

(defmethod get-random-move-cell ((this sih) person)
  (let* ((cells (get-move-cells this person)))
    (if cells
        (random-nth cells)
        nil)))

(defmethod move-person ((this sih) person to-row to-col)
  (let ((from-row (row person))
        (from-col (col person))
        (cells (cells this)))
    (setf (aref cells from-row from-col) nil)
    (setf (aref cells to-row to-col) person)
    (setf (row person) to-row)
    (setf (col person) to-col)
    (setf (rest-time person) (+ 0.5 (/ 1 (+ 1 (random 9)))))
    (setf (last-move-time person) (real-time-seconds))))

(defmethod move-persons ((this sih))
  "Check and move the person"
  (dolist (person (persons this))
    (with-slots (rest-time last-move-time) person
      (when (> (- (real-time-seconds) last-move-time) rest-time)
        (setf (last-move-time person) (real-time-seconds))
        (let* ((to (get-random-move-cell this person))
               (to-row (first to))
               (to-col (second to)))
          (when to
            (move-person this person to-row to-col)))))))


(defmethod gamekit:draw ((this sih))
  ;; (tick (person this))
  ;; (tick (policeman this))
  (move-persons this)
  (with-pushed-canvas ()
    (translate-canvas *padding-left* *padding-bottom*)
    (render this)
    (render (grid this))))

(defmethod run ()
  (gamekit:start 'sih))
