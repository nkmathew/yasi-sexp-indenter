(defun foo (x flag)
  (macrolet ((fudge (z)
               ;The parameters x and flag are not accessible
               ; at this point; a reference to flag would be to
               ; the global variable of that name.
               `(if flag (* ,z ,z) ,z)))
    ;The parameters x and flag are accessible here.
    (+ x
       (fudge x)
       (fudge (+ x 1)))))
