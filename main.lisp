(cl:in-package :sih)

(defvar *width* 800)
(defvar *height* 600)

(defvar *cursor-pos* (gamekit:vec2 0 0))

(defparameter *padding-bottom* 20)
(defparameter *grid-height* (- *height* (* 2 *padding-bottom*)))
(defparameter *rows* 9)
(defparameter *cell-size* (/ *grid-height* *rows*))
(defparameter *cell-half* (/ *cell-size* 2))
(defparameter *cols* (floor (/ (- *width* (* 2 *padding-bottom*)) *cell-size*)))
(defparameter *padding-left* (/ (- *width* (* *cell-size* *cols*)) 2))
(defparameter *grid-width* (- *width* (* 2 *padding-left*)))
(defparameter *cols* (floor (/ *grid-width* *cell-size*)))
(defparameter *once* nil)

(defvar *black* (gamekit:vec4 0 0 0 1))

(gamekit:defgame sih ()
  ((grid :initform (make-instance 'grid :rows *rows* :cols *cols* :cell-size *cell-size*) :accessor grid)
   (actors :initform '() :accessor actors)
   (cells :accessor cells))

  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Society is Hard"))

(defmethod post-initialize ((this sih))
  (setf (cells this) (make-array (list *rows* *cols*) :initial-element nil))
  (spawn-actor this 'actor)
  (spawn-actor this 'actor)
  (spawn-actor this 'policeman)
  (spawn-actor this 'actor)
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
  (format t "~& ~A x ~A ~%" row col))

(defmethod get-empty-cells ((this sih))
  (let ((result '()))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when (null (aref (cells this) row col))
          (push (list row col) result))))
    result))

(defmethod random-empty-cell ((this sih))
  (random-nth (get-empty-cells this)))

(defmethod spawn-actor ((this sih) type)
  (let ((actor (make-instance type))
        (free-pos (random-empty-cell this)))
    (push actor (actors this))
    (setf (row actor) (first free-pos))
    (setf (col actor) (second free-pos))
    (setf (aref (cells this) (first free-pos) (second free-pos)) actor)))

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

(defmethod get-move-cells ((this sih) actor)
  (let* ((result-cells '())
         (row (row actor))
         (col (col actor))
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

(defmethod get-random-move-cell ((this sih) actor)
  (let* ((cells (get-move-cells this actor)))
    (if cells
        (random-nth cells)
        nil)))

(defmethod move-actor ((this sih) actor to-row to-col)
  (let ((from-row (row actor))
        (from-col (col actor))
        (cells (cells this)))
    (setf (aref cells from-row from-col) nil)
    (setf (aref cells to-row to-col) actor)
    (setf (row actor) to-row)
    (setf (col actor) to-col)
    (setf (rest-time actor) (+ 0.5 (/ 1 (+ 1 (random 9)))))
    (setf (last-move-time actor) (real-time-seconds))))

(defmethod move-actors ((this sih))
  "Check and move the actors"
  (dolist (actor (actors this))
    (with-slots (rest-time last-move-time) actor
      (when (> (- (real-time-seconds) last-move-time) rest-time)
        (setf (last-move-time actor) (real-time-seconds))
        (let* ((to (get-random-move-cell this actor))
               (to-row (first to))
               (to-col (second to)))
          (when to
            (move-actor this actor to-row to-col)))))))


(defmethod gamekit:draw ((this sih))
  ;; (tick (actor this))
  ;; (tick (policeman this))
  (move-actors this)
  (with-pushed-canvas ()
    (translate-canvas *padding-left* *padding-bottom*)
    (render this)
    (render (grid this))))

(defmethod run ()
  (gamekit:start 'sih))
