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

(indent-files)
(exit)
