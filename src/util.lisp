(cl:in-package :sih)

(defmacro random-nth (lst)
  `(when ,lst
     (nth (random (length ,lst)) ,lst)))

(defun asset-path (pathname)
  (asdf:system-relative-pathname :sih (merge-pathnames pathname "assets/")))
