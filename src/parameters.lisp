(in-package :sil-game)

(defvar *black* (vec4 0 0 0 1))

(defparameter *grid-thickness-to-rows-factor* 150)
(defparameter *grid-thickness* nil)

(defparameter *grid-base-color* (vec3 0.0 0.0 0.0))
(defparameter *grid-color* nil)

(defparameter *path-color* (vec4 0 0 0 0.7))

(defparameter *button-color* (vec4 0.8 0.8 0.8 1.0))

(defparameter *status-bar-healthy-color* (vec4 0.1 0.9 0.1 1))
(defparameter *status-bar-sick-color* (vec4 0.9 0.1 0.1 1))
(defparameter *status-bar-dead-color* *button-color*)
(defparameter *status-bar-stroke-color* (vec4 0.0 0.0 0.0 1.0))
