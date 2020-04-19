(in-package :sil-game)

(defvar *black* (vec4 0 0 0 1))

(defparameter *background-color* (vec4 0.8 0.8 0.8 1))

(defparameter *grid-border-color* (vec4 0 0 0 0.3))
(defparameter *grid-border-thickness* 3)

(defparameter *grid-thickness-to-rows-factor* 150)
(defparameter *grid-thickness* nil)

(defparameter *grid-base-color* (vec3 0.0 0.0 0.0))
(defparameter *grid-color* nil)

(defparameter *path-color* (vec4 0.1 0.1 0.1 0.6))
(defparameter *button-color* (vec4 0.75 0.75 0.75 1.0))

(defparameter *status-bar-healthy-color* (vec4 0.5 0.86 0.02 1))
(defparameter *status-bar-sick-color* (vec4 0.77 0.03 0.03 1))
(defparameter *status-bar-dead-color* *grid-border-color*)
(defparameter *status-bar-stroke-color* (vec4 0.0 0.0 0.0 1.0))

(defparameter *quarantine-border-color* (vec4 0.5 0.1 0.1 1.0))
(defparameter *quarantine-border-thickness* 2)
