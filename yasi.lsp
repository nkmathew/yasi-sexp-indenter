#!/usr/bin/newlisp
;; @author nkmathew <kipkoechmathew@gmail.com>

;; Load 'yasi-module.lsp' using the absolute path so that running it from another
;; directory other than the project folder root will not raise an error like:
;; ERR: problem accessing file in function load : "yasi-module.lsp"

;; Returns the directory of the currently running script
(define (script-dir script-name)
 (let (script-path (real-path (main-args 1)))
  (replace (string script-name "\$") script-path "" 0)))

(load (string (script-dir "yasi.lsp") "yasi-module.lsp"))

;; newlisp includes everything typed in the command line in $main-args
;; ("newlisp" "yasi.lsp")
(when (> (length $main-args) 3)
  (indent-file ($main-args 2))
  (exit))

(when (= (length $main-args) 2)
  (print *help*)
  (exit))
