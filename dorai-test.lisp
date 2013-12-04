;; 1: Head form followed by other form
(some-user-function-1 arg1
                      arg2
                      ...)
;; 2: Head form is a list on a line by itself
((some-user-function-2)
 arg1
 arg2
 ...)

;; 3: Head form is a symbol on a line by itself
;; I think Dorai Sitaram made a mistake here. arg1 and arg2 should
;; line up under letter s and not o. His indenter does that and so
;; does yasi. 
(some-user-function-3
 arg1
 arg2
 ...)

;; 4: If the head form can be deduced to be a symbol.
;; yasi doesn't indent that way. The 4 in the second line should
;; line up under 2. Come to think of it, this is not valid Lisp code
(1 2 3
   4 5 6)

'(alpha
  beta
  gamma)
