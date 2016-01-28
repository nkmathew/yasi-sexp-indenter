
(setq table (make-hash-table))
(dotimes (i 10) (setf (gethash i table) i))

(print (let ((sum-of-squares 0))
         (maphash #'(lambda (key val)
                      (let ((square (* val val)))
                        (incf sum-of-squares square)
                        (setf (gethash key table) square)))
                  table)
         sum-of-squares))
