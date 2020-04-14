(cl:in-package :sih)

(defparameter *grid-thickness* 1)
(defparameter *grid-color* (vec4 0 0 0 0.3))

(defmacro random-nth (lst)
  `(when ,lst
     (nth (random (length ,lst)) ,lst)))

(defun asset-path (pathname)
  (asdf:system-relative-pathname :sih (merge-pathnames pathname "assets/")))
