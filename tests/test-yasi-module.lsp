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
  (assert=
   (find-line-ending "hello\r world\r")
   *CR*))

(define-test (test_cr_line_ending)
  (assert=
   (find-line-ending "hello\r world\r")
   *CR*))

(define-test (test_lf_line_ending)
  (assert=
   (find-line-ending "hello\n world\n")
   *LF*))

(define-test (test_crlf_line_ending)
  (assert=
   (find-line-ending "hello\r\n world\r\n")
   *CRLF*))

(define-test (test_cr_lf_mixed_line_ending)
  (assert=
   (find-line-ending "hello\r world \n mixed\r line endings\n")
   *CR*))

(define-test (test_find_line_ending_only_lf)
  (assert=
   (find-line-ending "First Line\n Second Line\n")
   *LF*))

(define-test (test_find_line_ending_only_crlf)
  (assert=
   (find-line-ending "First Line\r\n Second Line\r\n")
   *CRLF*))

(define-test (test_find_line_ending_only_cr)
  (assert=
   (find-line-ending "First Line\r Second Line\r")
   *CR*))

(define-test (test_find_line_ending_lf_with_cr_and_crlf)
  (assert=
   (find-line-ending "First Line\r Second Line\r\n Third Line\n Fourth Line \r\n")
   *CRLF*))

(define-test (test_find_line_ending_lf_with_crlf)
  (assert=
   (find-line-ending "First Line\n Second Line\r\n Third Line\n Fourth Line \r\n")
   *CRLF*))

(define-test (test_find_line_ending_cr_with_crlf)
  (assert=
   (find-line-ending "First Line\r Second Line\r\n Third Line\r Fourth Line \r\n")
   *CRLF*))

(define-test (test_find_line_ending_cr_with_lf)
  (assert=
   (find-line-ending "First Line\r Second Line\n Third Line\n Fourth Line \n\r")
   *CR*))

(define-test (test_find_line_ending_should_default_to_lf)
  (assert=
   (find-line-ending "Line without ending")
   *LF*))

(define-test (test_all_whitespace_tabs_only)
  (assert=
   (all-whitespace? "								")
   true))

(define-test (test_all_whitespace_tabs_and_spaces)
  (assert=
   (all-whitespace? "	    	    	    	    	    	    		")
   true))

(define-test (test_all_whitespace_empty_string)
  (assert=
   (all-whitespace? "")
   true))

(define-test (test_all_whitespace_no_whitespace)
  (assert=
   (all-whitespace? "NoSpace")
   nil))

(define-test (test_all_whitespace_spaces_only)
  (assert=
   (all-whitespace? "           ")
   true))

(UnitTest:run-all 'MAIN)
(exit)
