#!/usr/bin/newlisp
;; @author nkmathew <kipkoechmathew@gmail.com>
;; @date 20th November 2013

;; Translated from the python version.

(define __version__ "0.1.0")

(define [backup-dir] 0)
(define [default-indent] 1)
(define [dialect] 2)
(define [files] 3)
(define [indent-comments] 4)
(define [backup] 5)
(define [compact] 6)
(define [warning] 7)
(define [modify] 8)
(define [output] 9)
(define [uniform] 10)

(define [message] 0)
(define [line] 1)
(define [column] 2)

(define (parse-args (arguments))
  " Reads command-line arguments. The arguments can be in a flat list or a string
    Commandline arguments will instead be read from if no arguments are passed in
    the function call
  "
  (letn ((bool? (lambda (val)
                  (or (= nil val) (= true val))))
         (to-integer
          (lambda (str (default-num 1))
            (let (num (int str))
              (if (null? num)
                  default-num
                num))))
         (option-arg?
          (lambda (arg)
            (or
             (zero? (find "--dialect" arg))
             (zero? (find "-bd" arg))
             (zero? (find "--backup-dir" arg))
             (zero? (find "-nb" arg))
             (zero? (find "--no-backup" arg))
             (zero? (find "-nc" arg))
             (zero? (find "--no-compact" arg))
             (zero? (find "-no" arg))
             (zero? (find "--no-output" arg))
             (zero? (find "-nm" arg))
             (zero? (find "--no-modify" arg))
             (zero? (find "-uni" arg))
             (zero? (find "--uniform" arg))
             (zero? (find "-ic" arg))
             (zero? (find "--indent-comments" arg)))))
         (option-list?
          (lambda (lst)
            (and
             (> (length lst) 8) ;; There are at least 8 options right now
             (list? lst)
             (string? (lst [backup-dir]))
             (number? (lst [default-indent]))
             (string? (lst [dialect]))
             (list? (lst [files]))
             (bool? (lst [indent-comments]))
             (bool? (lst [backup]))
             (bool? (lst [compact]))
             (bool? (lst [warning]))
             (bool? (lst [modify])))))
         (options (list (real-path) 1 "" '() nil nil true true true true nil))
         (arguments
          (if (string? arguments)
              (parse arguments)
            (if (list? arguments)
                arguments
              (if (null? arguments)
                  (slice $main-args 2)
                '())))))
    (if (option-list? arguments)
        arguments
      (let ((i 0) (option-arg-encountered nil))
        (while (!= i (length arguments))
          (set 'curr (arguments i))
          (set 'prev (arguments (if (zero? i) 0 (- i 1))))
          (++ i)
          (when (option-arg? curr)
            (set 'option-arg-encountered true))
          ;; (when (= prev curr)
          ;;   (set 'curr ""))
          (if (not option-arg-encountered)
              ;; Anything not an argument is considered the path of a file to be
              ;; formatted
              (unless (empty? curr)
                (push curr (options [files])))
            (let ((indent-pair
                   (if (or (and (null? (find "--default-indent" prev))
                                (zero? (find "--default-indent" curr)))
                           (and (null? (find "-di" prev))
                                (zero? (find "-di" curr))))
                       ;; The "--backup-dir" string is at the end of the arg list
                       (list curr "")
                     (if (or (zero? (find "--di" prev))
                             (zero? (find "--default-indent" prev)))
                         (list prev curr)
                       (list "" ""))))
                  (backup-dir-pair
                   (if (or (and (null? (find "--backup-dir" prev))
                                (zero? (find "--backup-dir" curr)))
                           (and (null? (find "-bd" prev))
                                (zero? (find "-bd" curr))))
                       ;; The "--backup-dir" string is at the end of the arg list
                       (list curr "")
                     (if (or (zero? (find "-bd" prev))
                             (zero? (find "--backup-dir" prev)))
                         (list prev curr)
                       (list "" ""))))
                  (dialect-pair
                   (if (and (null? (find "--dialect" prev))
                            (zero? (find "--dialect" curr)))
                       ;; The "--dialect" string is at the end of the arg list
                       (list curr "")
                     (if (zero? (find "--dialect" prev))
                         (list prev curr)
                       (list "" "")))))
              (when (not (empty? (indent-pair 0)) (empty? (indent-pair 1)))
                (let ((lst (parse (dialect-pair 0) "=")))
                  (if (= 1 (length lst))
                      ;; No characters after the equal sign (no dialect specified)
                      (setq (options [default-indent]) (to-integer (indent-pair 1) 1))
                    (setq (options [default-indent]) (to-integer (join (rest lst)
                                                                       "=") 1)))))
              (when (not (empty? (dialect-pair 0)) (empty? (dialect-pair 1)))
                (let ((lst (parse (dialect-pair 0) "=")))
                  (if (= 1 (length lst))
                      ;; No characters after the equal sign (no dialect specified)
                      (setq (options [dialect]) (dialect-pair 1))
                    (setq (options [dialect]) (join (rest lst) "=")))))
              (when (not (empty? (backup-dir-pair 0)) (empty? (backup-dir-pair 1)))
                (let ((lst (parse (backup-dir-pair 0) "=")))
                  (if (= 1 (length lst))
                      ;; No characters after the equal sign (no directory specified)
                      (setq (options [backup-dir]) (backup-dir-pair 1))
                    (setq (options [backup-dir]) (join (rest lst) "=")))))
              (case curr
                ("-nc" (setq (options [compact]) nil))
                ("--no-compact" (setq (options [compact]) nil))
                ("-nb" (setq (options [backup]) nil))
                ("--no-backup" (setq (options [backup]) nil))
                ("-nm" (setq (options [modify]) nil))
                ("--no-modify" (setq (options [modify]) nil))
                ("-uni" (setq (options [uniform]) true))
                ("--uniform" (setq (options [uniform]) true))
                ("-ic" (setq (options [indent-comments]) true))
                ("--indent-commens" (setq (options [indent-comments]) true))
                ("--no-warning" (setq (options [warning]) nil))
                ("--nw" (setq (options [warning]) nil))
                ))))
        options))))

(define (print-args arg)
  (let (arg-list (parse-args arg))
    (println "backup dir: "      (arg-list [backup-dir]))
    (println "default indent: "  (arg-list [default-indent]))
    (println "dialect: "         (arg-list [dialect]))
    (println "files: "           (arg-list [files]))
    (println "indent comments: " (arg-list [indent-comments]))
    (println "backup: "          (arg-list [backup]))
    (println "compact: "         (arg-list [compact]))
    (println "warning: "         (arg-list [warning]))
    (println "modify: "          (arg-list [modify]))
    (println "output: "          (arg-list [output]))
    (println "uniform: "         (arg-list [uniform]))
    (println "")))


(define (printf)
  (println (format (args 0) (rest (args)))))

;; ****************************************************************************************
(define *help* (string [text]
usage: yasi [-h] [-nc] [-nb] [-nm] [-nw] [-no] [-ne] [-o OUTPUT_FILE]
            [--dialect DIALECT] [-v] [-bd backup-dir] [-di default-indent]
            [-ic] [-uni]
            [files [files ...]]

Dialect-aware s-expression indenter

positional arguments:
  files                 List of files to be indented. Will indent from
                        standard input if no files are specified

optional arguments:
  -h, --help            show this help message and exit
  -nc, --no-compact     Do not compact the code, just indent
  -nb, --no-backup      Do not create a backup file even if --backup-dir is
                        specified
  -nm, --no-modify      Do not modify the file
  -nw, --no-warning     Do not display warnings
  -no, --no-output      Suppress output of the indented code
  -ne, --no-exit        Instructs the program not to exit when a warning is
                        raised.
  -o OUTPUT_FILE        Path/name of output file
  --dialect DIALECT     Use Scheme keywords
  -v, --version         Prints script version
  -bd backup-dir, --backup-dir backup-dir
                        The directory where the backup file is to be written
  -di default-indent, --default-indent default-indent
                        The indent level to be used in case a function's
                        argument is in the next line. Vim uses 2, the most
                        common being 1.
  -ic, --indent-comments
                        If true, comment lines will be indented possibly
                        messing with any deliberate comment layout
  -uni, --uniform       Dictates whether the if-clause and else-clause of an
                        if-likeblock should have the same indent level.
[/text]))


;; ****************************************************************************************
;; GLOBAL CONSTANTS::

(define *stderr* 2)
(define [character] 0)
(define [line-number] 1)
(define [bracket-pos] 2)
(define [indent-level] 3)
(define [func-name] 4)
(define [spaces] 5)

(define *CR* "\r")
(define *LF* "\n")
(define *CRLF* "\r\n")

(define *os-sep* (if (= ostype "Win32") "\\" "/"))

;; Keywords that indent by two spaces
(define *scheme-keywords*
  '("define" "local-odd?" "when" "begin" "case" "local-even?" "do"
    "call-with-bytevector-output-port" "call-with-input-file" "call-with-port"
    "call-with-current-continuation" "open-file-input-port" "call-with-port"
    "call-with-values" "call-with-output-file" "call-with-string-output-port"
    "define-syntax" "if" "let" "let*" "library" "unless" "lambda" "syntax-rules"
    "syntax-case" "let-syntax" "letrec*" "letrec" "let-values" "let*-values"
    "with-exception-handler" "with-input-from-file" "with-interrupts-disabled"
    "with-input-from-string" "with-output-to-file" "with-input-from-port"
    "with-output-to-string" "with-source-path" "with-syntax" "with-implicit"
    "with-error-handler" "module" "parameterize"))


(define *clojure-keywords*
  '("defn" "fn" "dorun" "doseq" "loop" "when" "let" "defmacro" "binding" "doto"
    "ns" ":import" "defstruct" "condp" "defmacro" "comment" "when" "when-let"
    "->" "->>" "extend-type" "reify" "binding" "when-not" "proxy" "dotimes"
    "try" "finally" "for" "letfn" "catch" "iterate" "while" "with-local-vars"
    "locking" "defmulti" "defmethod" "extend"
    ))


(define *lisp-keywords*
  '(":implementation" ":method" "case" "defclass" "defconstant" "defgeneric"
    "defimplementation" "define-condition" "define-implementation-package"
    "definterface" "defmacro" "defmethod" "defpackage" "defproject" "deftype"
    "defun" "defvar" "do-external-symbols" "dolist" "dotimes" "ecase" "etypecase"
    "flet" "handler-bind" "if" "lambda" "let" "let*" "print-unreadable-object"
    "macrolet" "defparameter" "with-slots" "typecase" "loop" "when" "prog1"
    "unless" "with-open-file" "with-output-to-string" "with-input-from-string"
    "block" "handler-case" "defstruct" "eval-when" "tagbody" "ignore-errors"
    "labels" "multiple-value-bind"
    ))


(define *newlisp-keywords*
  '("while" "if" "case" "dotimes" "define" "dolist" "catch" "throw" "lambda"
    "lambda-macro" "when" "unless" "letex" "letn" "begin" "dostring" "let" "letn"
    "doargs" "define-macro" "until" "do-until" "do-while" "for-all" "for"
    ))


;; Keywords that indent by one space
(define *one-space-indenters* '("call-with-port"))

;; The 'if' and 'else' part of an if block should have different indent levels so
;; that they can stand out since there's no else Keyword in Lisp/Scheme to make
;; this explicit. list IF_LIKE helps us track these keywords.
(define *if-like*
  '("if"))


(define (add-keywords dialect)
  (cond
   ((= "lisp" dialect)
    (set 'lisp-if-like
         (append
          '("multiple-value-bind" "destructuring-bind" "do" "do*") *if-like*))
    (list *lisp-keywords* lisp-if-like))
   ((= dialect "scheme")
    (set 'scheme-if-like (append '("with-slots" "do" "do*")  *if-like*))
    (list *scheme-keywords* scheme-if-like))
   ((= dialect "clojure")
    (list *clojure-keywords* *if-like*))
   ((= dialect "newlisp")
    (list *newlisp-keywords* *if-like*))
   ((= dialect "all")
    (list
     (append *lisp-keywords* *scheme-keywords*
             *clojure-keywords* *newlisp-keywords*) *if-like*))
   (true '(() ()))))

;; ---------------------------------------------------------------------------------------

(define (warning warning-message message-format options)
  (letn ((opts (parse-args options)))
    (when (opts [warning])
      (write *stderr* (format warning-message message-format)))))


(define (read-file! filename)
  "Exits if the filename provided does not exist"
  (if (file? filename)
      (read-file filename)
    (warning "--%s-- Exiting. Filename `%s' is not valid"
                   ;; Exit if the filename is invalid
             (list (current-time) filename))))


(define (current-time)
  "Returns the current local time in mm:ss:ms"
  (letn ((curr-time (now))
         (minutes (curr-time 4))
         (seconds (curr-time 5))
         (micro-seconds (curr-time 6))
         (west-of-gmt (/ (curr-time 9) 60))
         (to-string (lambda (time-val) ;; Add a zero before the digit if it's less than 10
                      (if (< time-val 10)
                          (string "0" time-val)
                        (string time-val)))))
    (format "%s:%s:%s" (to-string minutes) (to-string seconds) (slice (to-string micro-seconds) 0 2))))


(define (filename-from-path path)
  " Returns the filename by splitting the file path along a slash "
  (letn ((lst (parse path *os-sep*)))
    (if (null? lst) "" (last lst))))


(define (backup-source-file! filename options)
  "Creates a copy of the file to be indented"
  (set 'opts (parse-args options))
  (set 'backup-dir (opts [backup-dir]))
  (unless (file? filename)
    (warning "--%s-- Warning: File `%s' does not exist"
             (list (current-time) filename) (opts [warning]) true filename))
  (unless (directory? backup-dir)
    (warning "--%s-- `%s': Warning: Directory `%s' does not exist"
             (list (current-time) filename backup-dir) (opts [warning]) true filename))
  (let (backup-name (string backup-dir "\\" ;; Change to forward slash in Unix
                            ;; Build the backup file name
                            (filename-from-path filename) ".yasi.bak~"))
    (copy-file filename backup-name)))


(define (string-trim! str)
  " Trim the string making sure there's no whitespace where it's not necessary"
  ;; "(print(++ 1))" ==> "(print (++ 1))"
  (set 'str (replace [text]([^\\(\[,{@~`'^#])(\(|\[|{)[/text] str (string $1 " " $2) 0))
  ;; "(print (++ 1)(-- 1))" ==> "(print (++ 1) (-- 1))"
  (set 'str (replace "(\\)|\]|\})(\\[|\\(|\{)" str (string $1 " " $2) 0))
  ;; "(print 'hello     )" ==> "(print 'hello)"
  (set 'str (replace "[ \t]*(\\)|\]|})" str $1 0))
  ;; "(print    'hello     )" ==> "(print 'hello )"
  (set 'str (replace "[ \t]{2,}" str " " 0))
  ;; "( (lambda () (print 'yes)))" ==> "((lambda () (print 'yes)))"
  (set 'str (replace "(\\()[ \t]*(?=(\\())" str $1 0))
  (set 'str (replace "(\\[)[ \t]*(?=(\\[))" str $1 0))
  (set 'str (replace "({)[ \t]*(?=({))" str $1 0))
  ;; "( (lambda () (print 'yes) ) )" ==> "( (lambda () (print 'yes)))"
  (set 'str (replace "(\\])[ \t]*(?=(\\)))" str $1 0))
  (set 'str (replace "(\\))[ \t]*(?=(\\]))" str $1 0))
  (set 'str (replace "(})[ \t]*(?=(}))" str $1 0))
  ;; Remove leading whitespace
  (set 'str (replace "^[ \t]*" str "" 0))
  ;; " ' ( "  ==> " '("
  (set 'str (replace "('|`)[ \t]+(\\(|\\[|{)" str (string $1 $2) 0))
  str)


(define (rstrip str chars)
  " Keeps deleting the last character while that character is in chars "
  (while (find (last str) chars)
    (setf (last str) ""))
  str)


(define (lstrip str chars)
  " Like rstrip but from the left. "
  (while (find (first str) chars)
    (setf (first str) ""))
  str)


(define (strip str chars)
  (lstrip (rstrip str chars) chars))


(define (split-preserve str sep)
  " Split the string into a list but preserve the separator in
  every split string in the list without introducing new characters.
  "
  (let ((str-list (parse str sep))
        (map-separator (lambda (separator lst)
                         (map (lambda (_)
                                (string _ separator)) lst))))
    (if str-list ;; an empty list will raise an error with function 'last'
      (if (empty? (last str-list))
          (begin
            ;; restore the separator in the split strings
            (set 'str-list (map-separator sep str-list))
            ;; set the last string to nothing in order to prevent the string from growing
            (pop str-list -1)
            str-list)
        (let (str-list (map-separator sep str-list))
          (setf (last str-list) (rstrip (last str-list) sep))
          str-list)))))


(define (find-line-ending str)
  " Find the line ending of the file by simply testing for existence of
  the three possible line endings hoping that the line endings are not mixed
  up in the file"
  (letn ((CR "\r")
         (LF "\n")
         (CRLF "\r\n"))
    (cond
     ((find CRLF str) CRLF)
     ((find CR str) CR)
     (true LF))))


(define (is-macro-name? func-name dialect)
  " Going to be used to determine whether the form should be indented by two spaces- since
  almost all macros do- using the dialect's conventions. "
  (and (cond
        ((null? func-name) nil)
        ((= dialect "lisp")     (regex "^(macro|def|do|with-)" func-name 0))
        ((= dialect "scheme")   (regex "^(call-|def|with-)" func-name 0))
        ((= dialect "clojure")  (regex "^(def|with)" func-name 0))
        ((= dialect "newlisp")  (regex "^(macro|def)" func-name 0)))
       true)) ;; Return true if any of the regexes above match


(define (all-whitespace? str)
  " Returns true if the line has only whitespace to the end. Such a line
   should not be messed with. If you don't want any whitespace to be preserved,
   make the function return true for every value"
  (and (regex "^[ \t]*(\r|\n|$)" str 0) true))


(define (find-trim-limit str)
  " Returns the maximum index to stop trimming at so that we don't alter the structure
  of strings ot comments that might have been layed out in some manner"
  (letn ((comment-start (regex {([^\\];)|(^;)} str 0)) ;; find first semicolon
         (limit (regex {([^\\]")|(^")} str 0)) ;; find first double quote
         (comment-start (if comment-start ;; store -1 if no semicolon is found
                          (+ (comment-start 2) (comment-start 1))
                          -1))
         (limit (if limit ;; store -1 if no double quote is found
                  (+ (limit 2) (limit 1))
                  -1)))
    (unless (= -1 comment-start)
      ;; Include all whitespace before the semi colon as part of the comment
      (set 'comment-start (++ ((regex "[ \t]*;" str) 1))))
    (if (and (!= -1 comment-start) (!= -1 limit))
        ;; Both a semicolon and a quote have been found
        (when (< comment-start limit)
          ;; The semicolon comes first, meaning the quote is part of the comment
          (set 'limit comment-start))
      (if (and (!= -1 comment-start) (= -1 limit))
          ;; A semicolon has been found but no quote
          (set 'limit comment-start)
        (if (= -1 limit)
            ;; Make sure -1 is not returned
            (set 'limit (length str)))))
    limit))


(define (pad-leading-whitespace str zero-level compact? blist)
  " Indents the correct number of whitespace before the line using the current
  indent level and the zero level"
  (let ((str
         (if compact?
             (letn ((trim-limit (if (regex "^[ \t]*;" str 0)
                                    (length str)
                                  (find-trim-limit str)))
                    (substr-1 (slice str 0 trim-limit)) ;; split into two portions
                    (substr-2 (slice str trim-limit))
                    (substr-1 (string-trim! substr-1))) ;; strip the first portion
               (string substr-1 substr-2)) ;; join the portions
           (let (str (replace "^[ \t]+" str "" 0)) ;; pad with zero-level spaces if in nocompact mode
             (append (dup " " zero-level) str)))))
    (if blist
        (let (current-level ((first blist) 3))
          ;; indent according to the current indentation level without including
          ;; the zero-level whitespace added earlier.
          (list (append (dup " " (- current-level zero-level)) str) current-level))
      (list str 0))))


(define (indent-line zero-level bracket-list line in-comment? in-symbol-region?
                     options)
  (letn ((opts (parse-args options))
         (comment-line (if (opts [indent-comments])
                           nil
                         (regex "^[ \t]*;" line 0)))
         (leading-spaces (regex "^[ \t]+[^; )\n\r]" line))
         (zero-level
          (if (and (not (opts [compact])) (zero? zero-level)
                   (empty? bracket-list) (zero? in-comment?))
              (if leading-spaces (- (leading-spaces 2) 1) 0)
            zero-level)))
    (if in-symbol-region?
        (list zero-level line 0)
      (if (and (not comment-line) (not (all-whitespace? line)))
          (push zero-level
                (pad-leading-whitespace line zero-level (opts [compact]) bracket-list))
        (list zero-level line 0)))))


(define (find-first-arg-pos curr-pos str options)
  (let ((leading-spaces 0)
        (substr (slice str (+ 1 curr-pos)))
        (opts (parse-args options)))
    (if (regex "^[ \t]*($|\r)" substr)
        (list 1 leading-spaces) ;; Return
      (if (and (!= curr-pos (-- (length str))) (= " " (str (+ 1 curr-pos))))
          ;;; Found space after bracket. We must first find the function position and
          ;;; then after that the first argument
          (letn ((space-regex (regex " +[^)\\]]| \\)" substr))
                 (leading-spaces (if space-regex
                                     (-- (space-regex 2))
                                   0))
                 (space-end (if space-regex
                                (+ (space-regex 1) (space-regex 2))
                              0))
                 (arg-pos (regex " +([^)])|( *(\\(|\\[))"
                                 (slice substr space-end))))
            (if arg-pos
                (set 'arg-pos (-- (+ 1 leading-spaces (+ (arg-pos 1) (arg-pos 2)))))
              (set 'arg-pos (+ 1 leading-spaces)))
            (letn ((substr-1 (slice substr space-end))
                   (first-space (or (find " " substr-1) 0)))
              (if (regex "^[ \t]*(#\\||;|$|\r)" (slice substr
                                                       (+ first-space space-end)))
                  (list (+ leading-spaces (opts [default-indent])) leading-spaces) ;; Return if comment found
                (list arg-pos leading-spaces)))) ;; Return if first argument found
        (letn ((space-regex (regex " +([^)}\n\r])|( *(\\(|\\[|{))" substr))
               ;;; No space after bracket. The first argument is simply after the
               ;;; first group of whitespace
               (first-space (or (find " " substr) -1)))
          (if space-regex
              (set 'arg-pos (+ (space-regex 1) (space-regex 2)))
            (set 'arg-pos 1))
          (if (regex "^[\t ]*(;|$|\r)" (slice substr first-space))
              (list (+ (opts [default-indent]) leading-spaces) leading-spaces)
            (list arg-pos leading-spaces))))))) ;; Return


(define (pop-from-list bracket lst fname line real-pos offset options msg-stack)
  (set 'opts (parse-args options))
  (if lst
      (letn ((popped-list (pop lst))
             (popped-bracket (popped-list [character]))
             (popped-offset (popped-list [indent-level]))
             (line-number (popped-list [line-number]))
             (correct-closer
              (cond
               ((= bracket "]") "[")
               ((= bracket ")") "(")
               (true "{"))))
        (when (!= popped-bracket correct-closer)
          (push (list line-number popped-offset
                      (format "Bracket `%s' does not match `%s' at (%d, %d)"
                              popped-bracket bracket line real-pos)) msg-stack)))
    (let ()
      (push (list line (+ 1 offset)
                  (format "Unmatched closing bracket `%s'" bracket)) msg-stack)))
  (list lst msg-stack))


(define (push-to-list lst func-name bracket line offset first-arg-pos first-item
                      in-list-literal? leading-spaces options)
  (let ((position-list (list bracket line offset
                             (+ first-arg-pos offset) func-name 0))
        (opts (parse-args options))
        (keywords (add-keywords (opts [dialect])))
        (two-spacer (or (is-macro-name? func-name (opts [dialect]))
                        (find func-name (keywords 0)))))
    (cond
     ((or in-list-literal? (= bracket "{")
          (and (= (opts [dialect]) "clojure") (= bracket "[")))
      (setf (position-list [indent-level]) (+ 0 first-item)))
     ((find func-name (keywords 1))
      (setf (position-list [indent-level]) (+ leading-spaces (if (opts [uniform])
                                                                 (+ offset 2)
                                                               (+ offset 4)))))
     ((and (find func-name *one-space-indenters*)
           (not (empty? func-name))) (+ 1 leading-spaces offset))
     ((and two-spacer (not (empty? func-name)))
      (setf (position-list [indent-level]) (+ 2 leading-spaces offset))))
    (push position-list lst)
    (when (>= (length lst) 3)
      (let (parent-func ((lst 2) [func-name]))
        (when (find parent-func '("flet" "labels" "macrolet"))
          (setf ((first lst) [indent-level]) (+ 2 offset)))))
    lst))


(define (indent-code original-code options)
  (letn ((opts (parse-args options))
         (keywords (add-keywords (opts [dialect])))
         (in-comment? 0) ;; Multiline comment
         (in-newlisp-string? 0)
         (in-newlisp-tag-string? nil)
         (in-string? nil)
         (in-symbol-with-space? nil)
         (last-quote-location '())
         (last-symbol-location '())
         (first-tag-string '())
         (line-number 1)
         (newlisp-brace-locations '())
         (zero-level 0)
         (line-ending (find-line-ending original-code))
         (code-lines (split-preserve original-code line-ending))
         (indented-code "")
         (bracket-locations '())
         (comment-locations '())
         (message-stack '())
         (not-zero? (lambda (val)
                      (not (zero? val))))
         (in-symbol-region? (or in-newlisp-tag-string? in-string? (not-zero? in-comment?) in-symbol-with-space?
                                (not-zero? in-newlisp-string?))))
    (dolist (line code-lines)
      (letn ((escaped? nil)
             (curr-line line)
             (indent-result (indent-line zero-level bracket-locations line in-comment?
                                         in-symbol-region?))
             (curr-line (indent-result 1))
             (indent-level (indent-result 2))
             (offset 0))
        ;; The zero-level can't be placed in the let binding because it's not
        ;; supposed to be reset for every line.
        (setf zero-level (first indent-result))
        (set 'indented-code (append indented-code curr-line))
        (catch  ;; Closest thing to a break statement in case we encounter a semi colon
         (dostring (chr curr-line)
           (letn ((next-char (slice curr-line (+ 1 offset) 1))
                  (prev-char (if (zero? offset)
                                 ;; using slice to get the previous character
                                 ;; won't always work.
                                 ""
                               (nth (- offset 1) curr-line)))
                  (substr (slice curr-line (+ 1 offset)))
                  (curr-char (char chr)))  ;; chr will be an integer. Convert it to a character
             (if escaped?
                 (set 'escaped? nil)
               (begin
                 (when (and (= curr-char "\\") (not in-newlisp-tag-string?)
                            (zero? in-newlisp-string?))
                   ;; The backslash only escapes when not in raw strings
                   (set 'escaped? true))
                 (when (and (= curr-char ";") (not in-symbol-region?)
                            (not (and (= "#" prev-char)
                                      (= (opts [dialect]) "scheme"))))
                   (throw)) ;; Skip to the next line
                 (when (and (not (find (opts [dialect]) '("clojure" "newlisp")))
                            (= "|" curr-char) (not in-string?))
                   ;; Take care of multiline comments when the dialect is not
                   ;; Clojure or newLISP
                   (cond
                    ((and (= "#" prev-char) (not in-symbol-with-space?))
                     (++ in-comment?)
                     (push (list line-number offset) comment-locations))
                    ((and (not-zero? in-comment?) (= next-char "#"))
                     (-- in-comment?)
                     (pop comment-locations))
                    ((zero? in-comment?)
                     (if in-symbol-with-space?
                         (begin
                           (set 'last-symbol-location '())
                           (set 'in-symbol-with-space? nil))
                       (begin
                         (set 'last-symbol-location (list line-number offset))
                         (set 'in-symbol-with-space? true))))))
                 (when (not (or in-symbol-with-space? (not-zero? in-comment?) in-newlisp-tag-string?))
                   ;; Deal with strings here
                   (when (= "\"" curr-char)
                     (set 'last-quote-location (list line-number offset))
                     (set 'in-string? (if (not in-string?)
                                          true
                                        nil)))
                   (when (and (not in-string?) (= (opts [dialect]) "newlisp"))
                     (when (= "{" curr-char)
                       (push (list line-number offset) newlisp-brace-locations)
                       (++ in-newlisp-string?))
                     (when (= "}" curr-char)
                       (if newlisp-brace-locations
                           (pop newlisp-brace-locations)
                         (push (list "Attempt to close a non-existent newLISP string"
                                     line-number offset) message-stack))
                       (-- in-newlisp-string?))))
                 (when (and (= curr-char "[") (= (opts [dialect]) "newlisp") (zero? in-newlisp-string?) (not in-string?))
                   ;; Deal with newLISP's tag strings here
                   (when (regex "\\[text\\]" (slice curr-line offset 7))
                     (set 'in-newlisp-tag-string? true)
                     (if (not first-tag-string) ;; Keep track of the first [text] tag
                       (set 'first-tag-string (list line-number offset))))
                   (when (regex "\\[/text\\]" (slice curr-line offset 7))
                     (set 'in-newlisp-tag-string? nil)
                     (set 'first-tag-string '())))
                 (set 'in-symbol-region? (or in-newlisp-tag-string? in-string? (not-zero? in-comment?) in-symbol-with-space?
                                             (not-zero? in-newlisp-string?)))
                 (when (not in-symbol-region?)
                   (let ((real-position (- (+ (- offset zero-level) ((regex "^[ \t]*" line) 2))
                                           indent-level)))
                     (when (and (find curr-char '("(" "[" "{"))
                                (not (and (find curr-char '("[" "{")) (find (opts [dialect]) '("newlisp" "lisp")))))
                       ;; The very long test condition prevents counting
                       ;; square and curly brackets in lisp and newLISP
                       ;; as the start of lists.
                       (letn ((arg-pos (find-first-arg-pos offset curr-line))
                              (first-arg-pos (first arg-pos))
                              (leading-spaces (arg-pos 1))
                              (func-name
                               (lower-case (strip (slice substr 0 (- first-arg-pos 1)) ")]\t\n\r ")))
                              (in-list-literal? nil))
                         (when (regex "('|`|#)([ \t]*\\(|\\[)($|\r)" (slice curr-line 0 (+ offset 1)))
                           (set 'in-list-literal? true))
                         (when (regex "^[^ \t]+[ \t]*($|\r)" substr)
                           (set 'func-name (lower-case (strip substr ")]\t\n\r "))))
                         (when (or (empty? func-name) in-list-literal?)
                           (set 'func-name nil))
                         (when (find func-name '("define-macro" "defmacro"))
                           (letn ((end-of-space ((regex "^[ \t]*" substr) 2))
                                  (substr (slice substr end-of-space))
                                  (substr (strip
                                           (slice substr
                                                  (or ((regex "[ \t]*" substr) 1) -1)) " \n\r\t"))
                                  (macro-name (slice substr 0 (or (find " " substr) -1))))
                             (if (not (empty? macro-name))
                                 (push macro-name (keywords 0)))))
                         (set 'first-item (+ offset 1
                                             ((regex "[ \t]*"
                                                     (slice curr-line (+ 1 offset))) 2)))
                         (set 'bracket-locations
                              (push-to-list bracket-locations func-name curr-char
                                            line-number offset first-arg-pos
                                            first-item
                                            in-list-literal? leading-spaces
                                            options))))
                     (when (and (find curr-char '("]" ")" "}"))
                                (not (and (find curr-char '("}" "]"))
                                          (find (opts [dialect]) '("newlisp" "lisp")))))
                       (letn ((popped-lst (pop-from-list
                                           curr-char bracket-locations
                                           fname line-number real-position offset
                                           options message-stack)))
                         (set 'bracket-locations (first popped-lst))
                         (set 'message-stack (last popped-lst))))
                     (when (and bracket-locations (find curr-char '(" " "\t"))
                                (find ((first bracket-locations) [func-name])
                                      (keywords 1)))
                       (when (or (not (find prev-char '(" " "\t" "")))
                                 (not (regex "^[ \t]*(;|#\\||$|\r)" curr-line)))
                         (++ ((first bracket-locations) [spaces])))
                       (when (= 2 ((first bracket-locations) [spaces]))
                         (unless (opts [uniform])
                           (-- ((first bracket-locations) [indent-level]))
                           (-- ((first bracket-locations) [indent-level]))
                           (setf ((first bracket-locations) [spaces]) 999)))))))))
           (++ offset))))
      (++ line-number))
    (list newlisp-brace-locations in-string? in-comment? in-symbol-with-space?
          bracket-locations last-quote-location fpath original-code indented-code
          last-symbol-location comment-locations in-newlisp-tag-string?
          first-tag-string message-stack)))


(define (after-indentation indentation-state fpath options)
  (letn ((opts (parse-args options))
         (newlisp-brace-locations (first indentation-state))
         (in-string? (indentation-state 1))
         (in-comment? (indentation-state 2))
         (in-symbol-with-space? (indentation-state 3))
         (bracket-locations (indentation-state 4))
         (last-quote-location (indentation-state 5))
         (fpath (indentation-state 6))
         (original-code (indentation-state 7))
         (indented-code (indentation-state 8))
         (last-symbol-location (indentation-state 9))
         (comment-locations (reverse (indentation-state 10)))
         (in-newlisp-tag-string? (indentation-state 11))
         (first-tag-string (indentation-state 12))
         (message-stack (indentation-state 13))
         (fname (filename-from-path fpath)))

    (dolist (message message-stack)
      (warning "%s:%d:%d: %s" (push fname message) opts))

    (when bracket-locations
      (dolist (bracket (reverse bracket-locations))
        (let ((y (bracket 1))
              (x (bracket 2))
              (character (first bracket)))
          (warning "\n%s:%d:%d: Unmatched `%s'"
                   (list fname y x character) opts))))

    (when newlisp-brace-locations
      (dolist (brace (reverse newlisp-brace-locations))
        (warning "\n%s:%d:%d: You have an unclosed newLISP string"
                 (list  fname (first brace) (brace 1)) opts)))

    (when in-string?
      (warning "\n%s:%d:%d String extends to end-of-file"
               (push  fname  last-quote-location) opts))

    (when comment-locations
      (dolist (comment comment-locations)
        (warning "\n%s:%d:%d: Unclosed comment"
                 (list fname (first comment) (comment 1)) opts)))

    (when last-symbol-location
      (warning "\n%s:%d:%d: Unclosed symbol"
               (list fname
                     (first last-symbol-location) (last-symbol-location 1)) opts))

    (when in-newlisp-tag-string?
      (warning "\n%s:%d:%d: Tag string extends to end-of-file. "
               (push fname first-tag-string) opts))

    (if (= indented-code original-code)
        (warning
         "\n File `%s' has already been formatted. Leaving it unchanged. . .\n"
         fname opts)
      (begin
        (when (opts [output])
          (print indented-code))
        (when (opts [modify])
          (write-file fpath indented-code)
          ; (write-file "temp.yasi" indented-code)
          ; (delete-file fpath)
          ; (rename-file "temp.yasi" fpath)
          )))))


(define (indent-files arguments)
  (letn ((opts (parse-args arguments)))
    (dolist (fpath (opts [files]))
      (unless (empty? fpath)
        (letn ((fname (real-path fpath))
               (code (read-file! (or fname (filename-from-path fpath))))
               (indent-result (indent-code code "--dialect=lisp"))
               (backup-dir (opts [backup-dir])))
          (after-indentation indent-result fname)
          (when (and (opts [backup]) (not backup-dir))
            (backup-source-file! fname opts))
          (when (and backup-dir (opts [backup]))
            (backup-source-file! fname opts)))))))
