#!/usr/bin/newlisp
;; @author nkmathew <kipkoechmathew@gmail.com>

;; Loads a script using the absolute path to prevent import errors when the script
;; ran from a different directory
(define (import-file fname)
  (let ((script-path
         (parse
          (replace {\} (real-path (main-args 1)) "/") "/") -1))
    (pop script-path -1)
    (load (string (join script-path "/") "/" fname))))

(import-file "yasim.lsp")

;; newlisp includes everything typed in the command line in $main-args
;; ("newlisp" "yasi.lsp")
(when (> (length $main-args) 3)
  (indent-files))
  (exit))

(when (= (length $main-args) 2)
  (print *help*)
  (exit))
