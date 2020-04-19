(cl:in-package :sil-game)

;; resouces
(register-resource-package :keyword (asdf:system-relative-pathname :such-is-life "assets/"))

(define-image :person-male "person-male.png")
(define-image :person-female "person-female.png")

(define-image :killer "killer.png")
(define-image :killer-male-disguised "person-male-disguised.png")
(define-image :killer-female-disguised "person-female-disguised.png")

(define-image :police-male "police-male.png")
(define-image :police-female "police-female.png")
(define-image :medic "medic.png")

(define-image :prison "prison.png")
(define-image :hospital "hospital.png")

(define-image :tile "tile.png")

(define-image :ok-sign "ok-sign.png")
(define-image :not-ok-sign "not-ok-sign.png")

(define-sound :death "sounds/death.wav")
(define-sound :cough "sounds/cough.ogg")
(define-sound :kill "sounds/kill.wav")
(define-sound :lock "sounds/lock.ogg")
(define-sound :heal "sounds/heal.wav")
(define-sound :win "sounds/win.wav")

(defvar *cursor-pos* (gamekit:vec2 0 0))

(defparameter *width* 1024)
(defparameter *height* 768)

(defparameter *panel-width* 200)
(defparameter *stage-width* (- *width* *panel-width*))
(defparameter *stage-height* *height*)

(defparameter *padding-bottom* 20)
(defparameter *grid-height* (- *stage-height* (* 2 *padding-bottom*)))

(defparameter *rows* 20)
(defparameter *cell-size* (/ *grid-height* *rows*))
(defparameter *cell-half* (/ *cell-size* 2))
(defparameter *cols* (floor (/ (- *stage-width* (* 2 *padding-bottom*)) *cell-size*)))
(defparameter *padding-left* (/ (- *stage-width* (* *cell-size* *cols*)) 2))
(defparameter *grid-width* (- *stage-width* (* 2 *padding-left*)))
(defparameter *cols* (floor (/ *grid-width* *cell-size*)))

(defparameter *area-dragging-p* nil)
;; % padding between a cell and the image assets
(defparameter *cell-padding* (* 0.1 *cell-size*))

(gamekit:defgame sil-game ()
  ((grid :initform (make-instance 'grid :rows *rows* :cols *cols* :cell-size *cell-size*) :accessor grid)
   (persons :initform '() :accessor persons)
   (cells :accessor cells)
   (hospital :accessor hospital :initform nil)
   (quarantine-from :accessor quarantine-from :initform nil)
   (quarantine-to :accessor quarantine-to :initform nil)
   (start-time :accessor start-time :initform nil)
   (statistics :accessor statistics :initform nil)
   (state :accessor state :initform 'in-progress)
   (panel :accessor panel
          :initform (make-instance 'panel
                                   :origin (vec2 *stage-width* 0)
                                   :width *panel-width*
                                   :height *height*
                                   :padding *padding-bottom*)))

  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Such Is Life"))
