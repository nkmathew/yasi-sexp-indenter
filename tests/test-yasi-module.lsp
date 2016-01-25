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
(load (string (get-parent-path (script-dir "test-yasi-module.lsp")) "\\yasim.lsp"))
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
   (string-trim! "( ( ( ")
   "((("))

(define-test (test_trim_adjacent_function_and_argument_opening_bracket)
  (assert=
   "(print (+ 1 1))"
   (string-trim! "(print(+ 1 1))")))

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
   10))

(define-test (test_find_trim_limit_double_quote_after_comment_block)
  (assert=
   (find-trim-limit [text](list 1123 '  #|  " ); "  |# 542) [/text])
   19))

(define-test (test_find_trim_limit_double_quote_before_semi_colon)
  (assert=
   (find-trim-limit [text](list 1123 ' " ); " 542)[/text])
   14))

(define-test (test_find_trim_limit_comment_alone_in_newlisp)
  (assert=
   (find-trim-limit
    {thhjh h               jgjh             ;;            hjbjh}
    "--dialect=newlisp")
   26))

(define-test (test_find_trim_limit_newlisp_brace_string)
  (assert=
   (find-trim-limit
    [text](string         {   Hello world                }    " message"))[/text]
    "--dialect=newlisp")
   16))

(define-test (test_find_trim_limit_newlisp_brace_string_before_comment)
  (assert=
   (find-trim-limit
    [text](println {            Hello     World                   }) ;; jjj[/text]
    "--dialect=newlisp")
   9))

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
      (assert= (is-macro-name? func-name "newlisp") true))))

(define-test (test_is_macro_name_scheme_macros)
  (letn ((macro-list
          '()))
    (dolist (func-name macro-list)
      (assert= (is-macro-name? func-name "scheme") true))))

(define-test (test_is_macro_name_clojure_macros)
  (letn ((macro-list
          '("defmacro")))
    (dolist (func-name macro-list)
      (assert= (is-macro-name? func-name "clojure") true))))

(define-test (test_is_macro_name_lisp_macros)
  (letn ((macro-list
          '("defmacro"
            "define-macro"
            "defstruct")))
    (dolist (func-name macro-list)
      (assert= (is-macro-name? func-name "lisp") true))))

(define-test (test_parse_args)
  (assert= (parse-args "--dialect=newlisp -bd backups-folder --uniform -ic")
   '("backups-folder" 1 "newlisp" () true nil true nil true true true "" 2 true
     ".yasi.bak~" -1)))

(define-test (test_parse_args1)
 (assert= (parse-args
           (parse-args
            (parse-args "--dialect=newlisp -bd backups-folder --uniform -ic")))
   '("backups-folder" 1 "newlisp" () true nil true nil true true true "" 2 true
     ".yasi.bak~" -1)))

(define-test (test_parse_args2)
  (assert= (parse-args
            (parse-args "--dialect=newlisp -bd backups-folder --uniform -ic"))
   '("backups-folder" 1 "newlisp" () true nil true nil true true true "" 2 true
     ".yasi.bak~" -1)))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[ System Tests ]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define [before] 0)
(define [after] 1)
(define [options] 2)

(define system-tests
  '(("tests/cases/#1-if-expression.lisp"
     "tests/cases/#1-if-expression~.lisp"
     "--dialect=lisp")
    ("tests/cases/#2-multiple-value-bind.lisp"
     "tests/cases/#2-multiple-value-bind~.lisp"
     "--dialect=lisp")
    ("tests/cases/#3-multiple-value-bind.lisp"
     "tests/cases/#3-multiple-value-bind~.lisp"
     "--uniform --dialect=lisp")
    ("tests/cases/#4-flet-indentation.lisp"
     "tests/cases/#4-flet-indentation~.lisp"
     "--dialect=lisp")
    ("tests/cases/#5-looks-like-a-macro.lisp"
     "tests/cases/#5-looks-like-a-macro~.lisp"
     "--dialect=lisp")
    ("tests/cases/#6-default-indent.lisp"
     "tests/cases/#6-default-indent~.lisp"
     "--dialect=lisp --default-indent 2")
    ("tests/cases/#7-uniform-if-expression.lisp"
     "tests/cases/#7-uniform-if-expression~.lisp"
     "--dialect=lisp --uniform")
    ("tests/cases/#8-macrolet-special-operator.lisp"
     "tests/cases/#8-macrolet-special-operator~.lisp"
     "--dialect=lisp --indent-comments")
    ("tests/cases/#9-standard-emacs-form-indentation.lisp"
     "tests/cases/#9-standard-emacs-form-indentation~.lisp"
     "--dialect=all")
    ("tests/cases/#10-newlisp-hash-comment.lsp"
     "tests/cases/#10-newlisp-hash-comment~.lsp"
     "--dialect=newlisp")
    ("tests/cases/#11-gradual-space-reduction-reindentation.lsp"
     "tests/cases/#11-gradual-space-reduction-reindentation~.lsp"
     "--dialect=newlisp")
    ("tests/cases/#12-zero-level-hanging-indentation.lsp"
     "tests/cases/#12-zero-level-hanging-indentation~.lsp"
     "--dialect=newlisp --no-compact")
    ("tests/cases/#13-hanging-with-non-hanging.lsp"
     "tests/cases/#13-hanging-with-non-hanging~.lsp"
     "--dialect=newlisp --no-compact")
    ("tests/cases/#14-tabbed-indentation.lsp"
     "tests/cases/#14-tabbed-indentation~.lsp"
     "--dialect=newlisp --no-compact --tabs=4")
    ("tests/cases/#15-input-space-output-tabs.lisp"
     "tests/cases/#15-input-space-output-tabs~.lisp"
     "--dialect=lisp --no-compact --tabs=4")
    ("tests/cases/#16-lisp-flets-and-labels.lisp"
     "tests/cases/#16-lisp-flets-and-labels~.lisp"
     "--dialect=lisp")
    ("tests/cases/#17-clojure-letfn.clj"
     "tests/cases/#17-clojure-letfn~.clj"
     "--dialect=clojure")
    ("tests/cases/#18-letfn-binding-block-indentation-only.clj"
     "tests/cases/#18-letfn-binding-block-indentation-only~.clj"
     "--dialect=clojure")))

(define-test (test_system)
  (for (case-number 0 18)
    (letn ((test-case (system-tests case-number))
           (project-dir (get-parent-path (script-dir "test-yasi-module.lsp")))
           (before-path (string project-dir *os-sep* (test-case [before])))
           (after-path (string project-dir *os-sep* (test-case [after])))
           (options (string "--no-rc " (test-case [options]))))
      (set 'before (read-file! before-path))
      (set 'after (read-file! after-path))
      (set 'indent-result (indent-code before options))
      (set 'indented-code (indent-result 7))
      (assert= after indented-code)
      ;; (unless (= indented-code after)
      ;;   (println "\n>>> Test Failed: " (first test-case) "\n")
      ;;   (println "Returned: \n" indented-code)
      ;;   (println "Expected: \n" after))
      )))

(UnitTest:run-all 'MAIN)
;; (test_system)
(exit)
