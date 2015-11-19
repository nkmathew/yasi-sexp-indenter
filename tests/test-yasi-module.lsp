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

;; Unit test library settings
(setq UnitTest:*enable-term-color*       true) ;; use colors in console?
(setq UnitTest:*report-failed*           true) ;; report failed assertions?
(setq UnitTest:*report-passed*           nil)  ;; report passed assertions?
(setq UnitTest:*continue-after-failure*  nil)
(setq UnitTest:*verbose*                 nil)

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

(define-test (test_find_first_arg_pos1)
  (assert=
   (find-first-arg-pos 0 "(     list 'one-sheep 'two-sheep )")
   '(11 5)))

(define-test (test_find_first_arg_pos2)
  (assert=
   (find-first-arg-pos 0 "(    list 'one-sheep 'two-sheep )")
   '(10 4)))

(define-test (test_find_first_arg_pos3)
  (assert=
   (find-first-arg-pos 3 "   (    list 'one-sheep 'two-sheep )")
   '(10 4)))

(define-test (test_find_first_arg_pos_argument_in_next_line_no_trailing_space)
  (assert=
   (find-first-arg-pos 0 "(    list")
   '(5 4)))

(define-test (test_find_first_arg_pos_argument_in_next_line_no_spaces_before_func)
  (assert=
   (find-first-arg-pos 0 "(list     ")
   '(1 0)))

(define-test (test_find_first_arg_pos_argument_is_func_call)
  (assert=
   (find-first-arg-pos 0 "(list (* 12 13) (* 13 14)  ")
   '(6 0)))

(define-test (test_find_first_arg_pos_no_function1)
  (assert=
   (find-first-arg-pos 0 "(        ")
   '(1 0)))

(define-test (test_find_first_arg_pos_no_function2)
  (assert=
   (find-first-arg-pos 0 "(")
   '(1 0)))

(define-test (test_trim_separate_adjacent_opening_and_closing_brackets)
  (assert=
   (string-trim! ")(")
   ") ("))

(define-test (test_trim_space_between_succeeding_opening_brackets)
  (assert=
   (string-trim! "( ( (")
   "((("))

(define-test (test_trim_adjacent_function_and_argument_opening_bracket)
  (assert=
   (string-trim! "(print(+ 1 1))")
   "(print (+ 1 1))"))

(define-test (test_trim_space_between_succeeding_closing_brackets)
  (assert=
   (string-trim! ")  )  )")
   ")))"))

(define-test (test_trim_spaces_before_closing_brackets)
  (assert=
   (string-trim! "(print 12    )")
   "(print 12)"))

(define-test (test_trim_extra_whitespace)
  (assert=
   (string-trim! "(print       'this)")
   "(print 'this)"))

(define-test (test_trim_leading_whitespace)
  (assert=
   (string-trim! "       (exit)")
   "(exit)"))

(define-test (test_trim_spaces_between_quote_and_opening_bracket_in_list_literal)
  (assert=
   (string-trim! "'        (12 13 14)")
   "'(12 13 14)"))

(define-test (test_find_trim_limit_double_quote)
  (assert=
   (find-trim-limit "(list 1123 \" ) \" 542)")
   12))

(define-test (test_find_trim_limit_literal_double_quote)
 (let (code "(list #\\; #\\| #\\\")")
  (assert=
   (find-trim-limit code)
   (length code))))

(define-test (test_find_trim_limit_double_and_single_quote)
  (assert=
   (find-trim-limit [text](list 1123 ' " ) " 542)[/text])
   14))

(define-test (test_find_trim_limit_double_quote_after_semi_colon)
  (assert=
   (find-trim-limit [text](list 1123 ;     " )" ";" 542)[/text])
   11))

(define-test (test_find_trim_limit_double_quote_after_comment_block)
  (assert=
   (find-trim-limit [text](list 1123 '  #|  " ); "  |# 542) [/text])
   19))

(define-test (test_find_trim_limit_double_quote_before_semi_colon)
  (assert=
   (find-trim-limit [text](list 1123 ' " ); " 542)[/text])
   14))

(define-test (test_split_preserve_empty_lines_at_EOF)
  (assert=
   (split-preserve "Tengo una pregunta\nSobre todo \n en este mundo\n\n\n\n\n" "\n")
   '("Tengo una pregunta\n"
     "Sobre todo \n"
     " en este mundo\n"
     "\n"
     "\n"
     "\n"
     "\n")))

(define-test (test_split_preserve_no_line_ending_at_EOF)
  (assert=
   (split-preserve  "Tengo una pregunta\nSobre todo \n en este mundo" "\n")
   '("Tengo una pregunta\n"
     "Sobre todo \n"
     " en este mundo")))

(define-test (test_split_preserve_no_delimiter)
  (assert=
   (split-preserve  "Tengo una pregunta  Sobre todo    en este mundo  " "\n")
   '("Tengo una pregunta  Sobre todo    en este mundo  ")))

(define-test (test_split_preserve)
  (assert=
   (split-preserve "Tengo una pregunta\nSobre todo \n en este mundo\n" "\n")
   '("Tengo una pregunta\n"
     "Sobre todo \n"
     " en este mundo\n")))

(define-test (test_is_macro_name_newlisp_macros)
 (letn ((macro-list
         '("define-test"
           "define-macro")))
  (dolist (func-name macro-list)
   (assert= (is-macro-name? func-name "newLISP") true))))

(define-test (test_is_macro_name_scheme_macros)
 (letn ((macro-list
         '()))
  (dolist (func-name macro-list)
   (assert= (is-macro-name? func-name "Scheme") true))))

(define-test (test_is_macro_name_clojure_macros)
 (letn ((macro-list
         '("defmacro")))
  (dolist (func-name macro-list)
   (assert= (is-macro-name? func-name "Clojure") true))))

(define-test (test_is_macro_name_lisp_macros)
 (letn ((macro-list
         '("defmacro"
           "define-macro"
           "defstruct")))
  (dolist (func-name macro-list)
   (assert= (is-macro-name? func-name "Common Lisp") true))))

(UnitTest:run-all 'MAIN)
(exit)
