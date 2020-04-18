(cl:in-package :sih)

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

(defparameter *width* 1024)
(defparameter *height* 768)

(defparameter *panel-width* 200)
(defparameter *stage-width* (- *width* *panel-width*))
(defparameter *stage-height* *height*)

(defparameter *padding-bottom* 20)
(defparameter *grid-height* (- *stage-height* (* 2 *padding-bottom*)))

(defparameter *rows* 5)
(defparameter *cell-size* (/ *grid-height* *rows*))
(defparameter *cell-half* (/ *cell-size* 2))
(defparameter *cols* (floor (/ (- *stage-width* (* 2 *padding-bottom*)) *cell-size*)))
(defparameter *padding-left* (/ (- *stage-width* (* *cell-size* *cols*)) 2))
(defparameter *grid-width* (- *stage-width* (* 2 *padding-left*)))
(defparameter *cols* (floor (/ *grid-width* *cell-size*)))

(defparameter *area-dragging-p* nil)
;; % padding between a cell and the image assets
(defparameter *cell-padding* (* 0.1 *cell-size*))

(defparameter *once* nil)

(gamekit:defgame sih ()
  ((grid :initform (make-instance 'grid :rows *rows* :cols *cols* :cell-size *cell-size*) :accessor grid)
   (persons :initform '() :accessor persons)
   (cells :accessor cells)
   (hospital :accessor hospital :initform nil)
   (quarantine-from :accessor quarantine-from :initform nil)
   (quarantine-to :accessor quarantine-to :initform nil)
   (panel :accessor panel
          :initform (make-instance 'panel
                                   :origin (vec2 *stage-width* 0)
                                   :width *panel-width*
                                   :height *height*
                                   :padding *padding-bottom*)))

  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Such Is Life"))
