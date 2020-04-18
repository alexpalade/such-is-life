(in-package :sih)

;; g, h, f represent the cost values
(defmethod find-path ((game sih) from-row from-col to-row to-col)
  (let* ((result nil)
         (count 0)
         (next '())
         (visited '())
         (starting-distance (distance-between from-row
                                              from-col
                                              to-row
                                              to-col)))
    (push (list :row from-row
                :col from-col
                :g 0
                :h starting-distance
                :f starting-distance)
          next)

    (loop while (and next (< count 50000))
          do (progn
               ;; sort the next nodes, to get lowest f-cost node
               (setf next (sort next #'< :key (lambda (plist) (getf plist :f))))

               ;(when (and (equal (getf (first next) :row) to-row)
               ;          (equal (getf (first next) :col) to-col))
               ;  (return))
               (when (cells-adjacent-p (getf (first next) :row)
                                       (getf (first next) :col)
                                       to-row
                                       to-col)
                 (return))

               ;; for this node, get all free neighbors
               (let* ((current (first next))
                      (row (getf current :row))
                      (col (getf current :col))
                      (neighbors (get-near-free-cells-ignore game row col 'hospital)))

                 ;; add all neighbors to next, if not already
                 ;; update their f and g
                 ;; neighbor has form: '(row col)
                 (setf neighbors
                       (remove-if
                        (lambda (x) (node-from-plist (first x) (second x) visited))
                        neighbors))
                 (dolist (neighbor neighbors)
                   (let* ((neighbor-row (first neighbor))
                          (neighbor-col (second neighbor))
                          (g-score (1+ (getf current :g)))
                          (h-score (distance-between neighbor-row
                                                     neighbor-col
                                                     to-row
                                                     to-col))
                          (f-score (+ g-score h-score))
                          (neighbor-plist
                            (node-from-plist neighbor-row neighbor-col next)))

                     (if (null neighbor-plist)
                         ;; add neighbor to next
                         (push (list :row neighbor-row
                                     :col neighbor-col
                                     :from (list row col)
                                     :g g-score
                                     :h h-score
                                     :f f-score)
                               (cdr (last next)))
                         ;; ... or update existing values
                         (when (< f-score (getf neighbor-plist :f))
                           (setf (getf neighbor-plist :f) f-score)
                           (setf (getf neighbor-plist :g) g-score)
                           (setf (getf neighbor-plist :from) (list row col))))))

                 ;; remember we visited this node
                 (push current visited)

                 ;; done visiting the first in queue
                 (setf next (rest next))

                 (incf count))))

    (if (and next
             (cells-adjacent-p (getf (first next) :row)
                               (getf (first next) :col)
                               to-row
                               to-col))
        (do ((current (first next)))
            ((and
              (equal (getf current :row) from-row)
              (equal (getf current :col) from-col))
             T)
          (progn
            (let ((row (getf current :row))
                  (col (getf current :col)))
              (push (list row col) result))
            (setf current
                  (node-from-plist (first (getf current :from))
                                   (second (getf current :from))
                                   visited))))
        (setf result nil))
    result))

(defun node-from-plist (row col plist)
  (first
   (member
    (list :row row :col col)
    plist
    :test (lambda (a b)
            (and (equal (getf a :row) (getf b :row))
                 (equal (getf a :col) (getf b :col)))))))
