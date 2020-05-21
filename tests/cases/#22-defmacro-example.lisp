
; My macro. This is a sketch of what I want that doesn't work.
(defmacro hijack-f (body)
`(macrolet ((f (x)`(f (+ 1 ,x))))
,@body))

; Defined in the library, I don't want to deal with these    
(defmacro f (x) x)
(defun g (x) (f (* x x x)))

(print (g 43))
