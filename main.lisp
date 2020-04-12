(cl:in-package :sih)

(defparameter *width* 800)
(defparameter *height* 600)

(gamekit:defgame sih () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Society is Hard"))

(defmethod gamekit:draw ((this sih))
  (gamekit:draw-text "Hello, world!" (gamekit:vec2 300 300)))

(gamekit:start 'sih)
