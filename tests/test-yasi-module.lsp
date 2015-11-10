#!/usr/bin/newlisp

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Returns the parent folder of the supplied path
;; ;; "C:\\mine\\file\\program.lsp"  --> "C:\\mine\\file"
(define (get-parent-path path)
  (letn ((sep (if (= ostype "Win32")
                  "\\"
                "/"))
         (new-path (if (= (path -1) sep) (replace "\\\\$" path "" 0) path))
         (splits (slice (parse new-path sep) 0 -1)))
    (join splits sep)))

;; Returns the directory of the currently running script
(define (script-dir script-name)
 (let (script-path (real-path (main-args 1)))
  (replace (string script-name "\$") script-path "" 0)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Imports
(load (string (get-parent-path (script-dir "test-yasi-module.lsp")) "\\yasi-module.lsp"))
(load (string (script-dir "test-yasi-module.lsp") "nl-unittest.lsp"))

(context 'MAIN)

(define-test (test_cr_line_ending)
  (assert= (find-line-ending "hello\r world\r") "\r"))

(define-test (test_lf_line_ending)
  (assert= (find-line-ending "hello\n world\n") "\n"))

(define-test (test_crlf_line_ending)
  (assert= (find-line-ending "hello\r\n world\r\n") "\r\n"))

(define-test (test_cr_lf_mixed_line_ending)
  (assert= (find-line-ending "hello\r world \n mixed\r line endings\n") "\r"))

(UnitTest:run-all 'MAIN)
(exit)
