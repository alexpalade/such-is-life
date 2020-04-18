(cl:in-package :sih)

(defmacro random-nth (lst)
  `(when ,lst
     (nth (random (length ,lst)) ,lst)))

(defun asset-path (pathname)
  (asdf:system-relative-pathname :sih (merge-pathnames pathname "assets/")))

(defun valid-cell (row col num-rows num-cols)
  (and (>= row 0)
       (>= col 0)
       (< row num-rows)
       (< col num-cols)))

(defun invalid-cell (row col num-rows num-cols)
  (not (valid-cell row col num-rows num-cols)))

(defun get-near-cells (row col num-rows num-cols)
  (remove-if
   (lambda (xy)
     (invalid-cell (first xy) (second xy) num-rows num-cols))
   (map 'list
        (lambda (d)
          (list (+ row (first d)) (+ col (second d))))
        (list '(0 1) '(1 1) '(1 0) '(1 -1) '(0 -1) '(-1 -1) '(-1 0) '(-1 1)))))
