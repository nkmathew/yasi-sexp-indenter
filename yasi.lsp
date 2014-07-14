
;; @author    ng'etich mathew <kipkoechmathew@gmail.com>
;; @started   20th November 2013
;; @Finished  4th December 2013

;; Translated from the python version. 
;; It's faster than yasi.py by about 0.6 seconds. 

(define (lisp-dialect lst)
  "Tells the Lisp dialect specified from the list provided, usually
  command line arguments. Defaults to all if non is specified"
  (or (catch 
        (dolist (arg lst)
          (case arg
            ("--clojure" (throw "Clojure"))
            ("--lisp"    (throw "Common Lisp"))
            ("--newlisp" (throw "newLISP"))
            ("--scheme"  (throw "Scheme"))))) "All"))

(define (fprint) 
  (println (format (args 0) (rest (args)))))

;; **************************************************************************************** 
(define *help* (string [text]
 _________________________________________________________________________________________________________________
|    Usage: newlisp yasi.lsp [[<file>] [--backup-dir<directory>] [--no-compact] [--no-backup] [--no-warning]      |
|                   [--clojure] [--lisp] [--scheme] [--default-indent <num>]]                                     |
|            -nb,    --no-backup     # Don't create a backup file even if --backup-dir is specified               |
|            -nc,    --no-compact    # Try to preserve the structure of the file.                                 |
|            -nw,    --no-warning    # Don't issue warnings                                                       |
|            -ne,    --no-exit       # Instructs the program to exit when a warning is raised. True by default    |
|            -uni,   --uniform       # Dictates whether the if-clause and else-clause of an if-like block should  |
|                                       have the same indent level. False by default                              |
|            -no,    --no-output     # Suppress output of the indented code                                       |
|            -nm,    --no-modify     # Don't modify the file                                                      |
|            --backup-dir            # The directory where the backup file is to be written                       |
|            --clojure               # Use Clojure keywords                                                       |
|            --lisp                  # Use Lisp keywords                                                          |
|            --newlisp               # Use newLISP keywords                                                       |
|            --scheme                # Use Scheme keywords                                                        |
|            --default-indent <num>  # The indent level to be used in case a                                      |
|                                    function's argument is in the next line. Vim uses 2, the most common being 1.|
|            --indent-comments, -ic  # False by default. If true, comment lines will be indented possibly         |
|                                        messing with any deliberate comment layout                               |
+-----------------------------------------------------------------------------------------------------------------+
[/text]))

;; ****************************************************************************************
;; GLOBAL CONSTANTS::

(define *stderr* 2)
(define *character* 0)
(define *line-number* 1)
(define *bracket-pos* 2)
(define *indent-level* 3)
(define *func-name* 4)
(define *spaces* 5)

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

;; ------ Command line options: ----------------------------------------------

(define *backup*
  (if (or (find "--no-backup"  $main-args) (find "-nb" $main-args))
      nil
    true))

(define *exit*
  (if (or (find "--no-exit"    $main-args) (find "-ne" $main-args))
      nil
    true))

(define *output*
  (if (or (find "--no-output"  $main-args) (find "-no" $main-args))
      nil
    true))

(define *uniform*
  (if (or (find "--uniform"    $main-args) (find "-uni" $main-args))
      true
    nil))

(define *warn*
  (if (or (find "--no-warning" $main-args) (find "-nw" $main-args))
      nil
    true))

(define *modify*
  (if (or (find "--no-modify"  $main-args) (find "-nm" $main-args))
      nil
    true))

(define *compact*
  (if (or (find "--no-compact"  $main-args) (find "-nc" $main-args))
      nil
    true))


(define *indent-comments*
  (if (or (find "--indent-comments"  $main-args) (find "-ic" $main-args))
      true
    nil))

;; -----------------------------------------------------------------------------

;; The 'if' and 'else' part of an if block should have different indent levels so
;; that they can stand out since there's no else Keyword in Lisp/Scheme to make
;; this explicit.  list IF_LIKE helps us track these keywords.
(define *if-like* 
  '("if"
    ))

(define *dialect* (lisp-dialect $main-args))

(cond
 ((= "Common Lisp" *dialect*)
  (set '*two-space-indenters* *lisp-keywords*)
  (set '*if-like* (append '("multiple-value-bind" "destructuring-bind" "do" "do*")  *if-like*)))
 ((= *dialect* "Scheme")
  (set '*two-space-indenters* *scheme-keywords*)
  (set '*if-like* (append '("with-slots" "do" "do*")  *if-like*)))
 ((= *dialect* "Clojure")
  (set '*two-space-indenters* *clojure-keywords*)
  (set '*if-like* (append '() *if-like*)))
 ((= *dialect* "newLISP")
  (set '*two-space-indenters* *newlisp-keywords*)
  (set '*if-like* (append '() *if-like*)))
 ((= *dialect* "All")
  (set '*two-space-indenters* (append *lisp-keywords* *scheme-keywords*
                                      *clojure-keywords* *newlisp-keywords*))))

(define *default-indent* 1)
(let (pos (find "--default-indent" $main-args))
  (when pos
    (when (> (length $main-args) pos)
      (set '*default-indent* (or (int (main-args (++ pos))) 1)))))

;; ---------------------------------------------------------------------------------------


(define (read-file! filename)
  "Exits if the filename provided does not exist"
  (if (file? filename)
      (read-file filename)
    (issue-warning "--%s-- Exiting. Filename `%s' is not valid" 
                   (list (current-time) filename) *warn* true filename))) ;; Exit if the filename is invalid

(define (issue-warning warning-message message-format warn? exit-after-warning? fname)
  (when warn? 
    (write *stderr* (format warning-message message-format)))
  (when exit-after-warning?
    (write *stderr* (format "\n--%s-- Exiting. File `%s' unchanged. . .\n" (cons (current-time) fname)))
    (exit)))


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


(define (get-backup-directory lst fname)
  "Returns the backup directory from the command line argument list"
  (if (find "--backup-dir" lst)
      (let (backup-dir (lst (++ (find "--backup-dir" lst))))
        (if (directory? backup-dir)
            (real-path backup-dir) ;; Return the full path to the backup dir
          (issue-warning "\n--%s-- `%s' : Warning: The directory `%s' is unusable for backup. "
                         (list (current-time) fname backup-dir) *warn* *exit* fname)))
    nil))

(define (filename-from-path path)
  " Returns the filename by splitting the file path along a slash "
  (letn ((lst (parse path (if (= ostype "Win32")
                              "\\"
                            "/"))))
    (last lst)))


(define (backup-source-file! filename backup-dir exit-on-error?)
  "Creates a copy of the file to be indented"
  (unless (file? filename)
    (issue-warning "--%s-- Warning: File `%s' does not exist" (list (current-time) filename) *warn* true filename))
  (unless (directory? backup-dir)
    (issue-warning "--%s-- `%s': Warning: Directory `%s' does not exist" (list (current-time) filename backup-dir) *warn* true filename))
  (let (backup-name (string backup-dir {\} ;; Change to forward slash in Unix
                            ;; Build the backup file name
                            (filename-from-path filename) ".yasi.bak~"))
    (copy-file filename backup-name)))


(define (my-trim! str)
  " Trim the string making sure there's no whitespace where it's not necessary"
  ;; "(print(++ 1))" ==> "(print (++ 1))"
  (set 'str (replace [text] ([^\\(\[,{@~`'^#]) (\(|\[| {) [/text] str (string $1 " " $2) 0))
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
  (set 'str (replace " ('|`) (\\(|\\[|{)" str (string " " $1 $2) 0))
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
  "Split the string into a list but preserve the separator in
 every split string in the list without introducing new characters.
 "
  (let ((str-list (parse str sep))
        (map-separator (lambda (separator lst)
                         (map (lambda (_)
                                (string _ separator)) lst)))) 
    (if str-list ;; an empty list will raise an error with function 'last'
      (if (empty? (last str-list))
          (begin
            (set 'str-list (map-separator sep str-list)) ;; restore the separator in the split strings
            (setf (last str-list) "") ;; set the last string to nothing in order to prevent the string from growing
            str-list)
        (let (str-list (map-separator sep str-list))
          (setf (last str-list) (rstrip (last str-list) sep))
          str-list)))))

(define (find-line-ending str)
  "Find the line ending of the file by simply testing for existence of
  the three possible line endings hoping that the line endings are not mixed
  up in the file"
  (letn ((CR "\r")
         (LF "\n")
         (CRLF "\r\n"))
    (cond
     ((find CRLF str) CRLF)
     ((find CR str) CR)
     (true LF))))

(define (is-macro-name? form dialect)
  "Going to be used to determine whether the form should be indented by two spaces- since
  almost all macros do- using the dialect's conventions. "
  (and (cond 
        ((null? form) nil)
        ((= dialect "Common Lisp") (regex "macro|def|do|with-" form 0))
        ((= dialect "Scheme")      (regex "call-|def|with-" form 0))
        ((= dialect "Clojure")     (regex "def|with" form 0))
        ((= dialect "newLISP")     (regex "macro|def" form 0)))
       true)) ;; Return true if any of the regexes above match

(define (all-whitespace? str)
  "Returns true if the line has only whitespace to the end. Such a line
  should not be messed with. If you don't want any whitespace to be preserved,
  make the function return true for every value"
  (and (regex "^[ \t]*(\r|\n|$)" str 0) true))

(define (find-trim-limit str)
  "Returns the maximum index to stop trimming at so that we don't alter the structure 
  of strings ot comments that might have been layed out in some manner"
  (letn ((comment-start (regex {([^\\];)|(^;)} str 0)) ;; find first semicolon 
         (limit (regex {([^\\]")|(^")} str 0)) ;; find first doule quote
         (comment-start (if comment-start ;; store -1 if no semicolon is found
                          (+ 2 (comment-start 1))
                          -1))
         (limit (if limit ;; store -1 if no double quote is found
                  (+ 2 (limit 1))
                  -1)))
    (unless (= -1 comment-start)
      ;; Include all whitespace before the semi colon as part of the comment
      (set 'comment-start ((regex "[ \t]*;" str) 1)))
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
  "Indents the correct number of whitespace before the line using the current
indent level and the zero level"
  (let ((str
         (if compact?
             (letn ((trim-limit (if (regex "^[ \t]*;" str 0)
                                    (length str)
                                  (find-trim-limit str)))
                    (substr-1 (slice str 0 trim-limit)) ;; split into two portions
                    (substr-2 (slice str trim-limit))
                    (substr-1 (my-trim! substr-1))) ;; strip the first portion
               (string substr-1 substr-2)) ;; join the portions
           (let (str (replace "^[ \t]+" str "" 0)) ;; pad with zero-level spaces if in nocompact mode
             (append (dup " " zero-level) str)))))
    (if blist
        (let (current-level ((first blist) 3))
          ;; indent according to the current indentation level without including
          ;; the zero-level whitespace added earlier.
          (list (append (dup " " (- current-level zero-level)) str) current-level))
      (list str 0))))


(define (indent zero-level bracket-list line in-comment? in-symbol-region?)
  (letn ((comment-line (if *indent-comments*
                           nil
                         (regex "^[ \t]*;" line 0)))
         (leading-spaces (regex "^[ \t]+[^; )\n\r]" line))
         (zero-level 
          (if (and (not *compact*) (zero? zero-level) (empty? bracket-list) (zero? in-comment?))
              (if leading-spaces (- (leading-spaces 2) 1) 0)
            zero-level)))
    (if in-symbol-region? 
      (list zero-level line 0)
      (if (and (not comment-line) (not (all-whitespace? line)))
          (push zero-level (pad-leading-whitespace line zero-level *compact* bracket-list))
        (list zero-level line 0)))))


(define (find-first-arg-pos curr-pos str)
  (let ((leading-spaces 0)
        (substr (slice str (+ 1 curr-pos))))
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
                  (list (+ leading-spaces *default-indent*) leading-spaces) ;; Return if comment found
                (list arg-pos leading-spaces)))) ;; Return if first argument found
        (letn ((space-regex (regex " +([^)}\n\r])|( *(\\(|\\[|{))" substr))
               ;;; No space after bracket. The first argument is simply after the
               ;;; first group of whitespace
               (first-space (or (find " " substr) -1)))
          (if space-regex
              (set 'arg-pos (+ (space-regex 1) (space-regex 2)))
            (set 'arg-pos 1))
          (if (regex "^[\t ]*(;|$|\r)" (slice substr first-space))
              (list (+ *default-indent* leading-spaces) leading-spaces)
            (list arg-pos leading-spaces))))))) ;; Return 


(define (pop-from-list bracket lst fname line real-pos offset)
  (if lst
      (letn ((popped-list (pop lst))
             (popped-bracket (popped-list *character*))
             (popped-offset (popped-list *indent-level*))
             (line-number (popped-list *line-number*))
             (correct-closer 
              (cond
               ((= bracket "]") "[")
               ((= bracket ")") "(")
               (true "{"))))
        (when (!= popped-bracket correct-closer)
          (issue-warning "\n--%s-- %s: Warning: Bracket `%s' at (%d, %d) does not match `%s' at (%d, %d)"
                         (list (current-time) fname popped-bracket line-number popped-offset bracket line real-pos)
                         *warn* *exit* fname)))
    (let ((bpos 
           (if *exit*
               (+ 1 real-pos)
             (+ 1 offset))))
      (issue-warning "\n--%s-- %s: Warning: Unmatched `%s' near (%d, %d). "
                     (list (current-time) fname bracket line bpos) *warn* *exit* fname)))
  lst)


(define (push-to-list lst func-name bracket line offset first-arg-pos first-item
                      in-list-literal? leading-spaces)
  (let ((position-list (list bracket line offset 
                             (+ first-arg-pos offset) func-name 0))
        (two-spacer (or (find func-name *two-space-indenters*)
                        (is-macro-name? func-name *dialect*))))
    (cond
     ((or in-list-literal? (= bracket "{")
          (and (= *dialect* "Clojure") (= bracket "[")))
      (setf (position-list *indent-level*) (+ 0 first-item)))
     ((find func-name *if-like*)
      (setf (position-list *indent-level*) (+ leading-spaces (if *uniform*
                                                                 (+ offset 2)
                                                               (+ offset 4)))))
     ((and (find func-name *one-space-indenters*)
           (not (empty? func-name))) (+ 1 leading-spaces offset))
     ((and two-spacer (not (empty? func-name)))
      (setf (position-list *indent-level*) (+ 2 leading-spaces offset))))
    (push position-list lst)
    (when (>= (length lst) 3)
      (let (parent-func ((lst 2) *func-name*))
        (when (find parent-func '("flet" "labels" "macrolet"))
          (setf ((first lst) *indent-level*) (+ 2 offset)))))
    lst))



(define (indent-code original-code fpath)
  (letn ((fname (filename-from-path fpath))
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
         (not-zero? (lambda (val)
                      (not (zero? val))))
         (in-symbol-region? (or in-newlisp-tag-string? in-string? (not-zero? in-comment?) in-symbol-with-space? 
                                (not-zero? in-newlisp-string?))))
    (dolist (line code-lines)
      (letn ((escaped? nil)
             (curr-line line)
             (indent-result (indent zero-level bracket-locations line in-comment?
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
                 (when (and (= curr-char "\\") (not in-newlisp-tag-string?) (zero? in-newlisp-string?))
                   (set 'escaped? true)) ;; The backslash only escapes when not in raw strings
                 (when (and (= curr-char ";") (not in-symbol-region?) (not (and (= "#" prev-char) (= *dialect* "Scheme"))))
                   (throw)) ;; Skip to the next line
                 (when (and (not (find *dialect* '("Clojure" "newLISP"))) (= "|" curr-char) (not in-string?))
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
                     (set 'last-quote-location (list fname line-number offset))
                     (set 'in-string? (if (not in-string?)
                                          true
                                        nil)))
                   (when (and (not in-string?) (= *dialect* "newLISP"))
                     (when (= "{" curr-char)
                       (push (list line-number offset) newlisp-brace-locations)
                       (++ in-newlisp-string?))
                     (when (= "}" curr-char)
                       (if newlisp-brace-locations
                           (pop newlisp-brace-locations)
                         (issue-warning "--%s-- `%s': Warning: Attempt to close a non-existent string\n" 
                                        (list (current_time) fname)
                                        *warn* *exit* fname))
                       (-- in-newlisp-string?))))
                 (when (and (= curr-char "[") (= *dialect* "newLISP") (zero? in-newlisp-string?) (not in-string?))
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
                                (not (and (find curr-char '("[" "{")) (find *dialect* '("newLISP" "Common Lisp")))))
                        ;; The very long test condition prevents counting
                        ;; square and curly brackets in Common Lisp and newLISP
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
                                  (substr (strip (slice substr (or ((regex "[ \t]*" substr) 1) -1)) " \n\r\t"))
                                  (macro-name (slice substr 0 (or (find " " substr) -1))))
                             (if (not (empty? macro-name))
                                 (push macro-name *two-space-indenters*))))
                         (set 'first-item (+ offset 1 ((regex "[ \t]*" (slice curr-line (+ 1 offset))) 2)))
                         (set 'bracket-locations
                              (push-to-list bracket-locations func-name curr-char line-number offset first-arg-pos first-item
                                            in-list-literal? leading-spaces))))
                     (when (and (find curr-char '("]" ")" "}"))
                                (not (and (find curr-char '("}" "]")) (find *dialect* '("newLISP" "Common Lisp")))))
                       (set 'bracket-locations (pop-from-list curr-char bracket-locations fname line-number real-position offset)))
                     (when (and bracket-locations (find curr-char '(" " "\t")) (find ((first bracket-locations) *func-name*) *if-like*))
                       (when (or (not (find prev-char '(" " "\t" ""))) (not (regex "^[ \t]*(;|#\\||$|\r)" curr-line)))
                         (++ ((first bracket-locations) *spaces*)))
                       (when (= 2 ((first bracket-locations) *spaces*))
                         (unless *uniform* 
                           (-- ((first bracket-locations) *indent-level*))
                           (-- ((first bracket-locations) *indent-level*))
                           (setf ((first bracket-locations) *spaces*) 999)))))))))
           (++ offset))))
      (++ line-number))
    (list newlisp-brace-locations in-string? in-comment? in-symbol-with-space? bracket-locations last-quote-location
          fpath original-code indented-code last-symbol-location comment-locations in-newlisp-tag-string? first-tag-string)))

(define (after-indentation indentation-state)
  (letn ((newlisp-brace-locations (first indentation-state))
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
         (fname (filename-from-path fpath)))
    (when bracket-locations
      (dolist (bracket (reverse bracket-locations))
        (let ((y (bracket 1))
              (x (bracket 2))
              (character (first bracket)))
          (issue-warning "\n--%s-- `%s': Warning : Unmatched `%s' near (%d, %d). "
                         (list (current-time) fname character y x)
                         *warn* *exit* fname))))

    (when newlisp-brace-locations
      (dolist (brace (reverse newlisp-brace-locations))
        (issue-warning "\n--%s-- `%s': Warning: You have an unclosed newLISP string starting from (%d, %d)"
                       (list (current-time) fname (first brace) (brace 1)) *warn* *exit* fname)))

    (when in-string?
      (issue-warning "\n--%s-- `%s': Warning: The string starting from (%d, %d) extends to end-of-file. "
                     (push (current-time) last-quote-location) *warn* *exit* fname))
    (when comment-locations
      (dolist (comment comment-locations)
        (issue-warning "\n--%s-- `%s': Warning: Unclosed comment near (%d, %d)"
                       (list (current-time) fname (first comment) (comment 1)) *warn* *exit* fname)))

    (when last-symbol-location
      (issue-warning "\n--%s-- `%s': Warning: Unclosed symbol near (%d, %d). "
                     (list (current-time) fname (first last-symbol-location) (last-symbol-location 1))
                     *warn* *exit* fname))

    (when in-newlisp-tag-string?
      (issue-warning "\n--%s-- `%s': Warning: The tag string starting from (%d, %d) extends to end-of-file. "
                     (append (list (current-time) fname) first-tag-string) *warn* *exit* fname))

    (if (= indented-code original-code)
        (issue-warning "\n--%s-- File `%s' has already been formatted. Leaving it unchanged. . .\n" 
                       (list (current-time) fname)
                       *warn* nil fname)
      (begin
        (when *output*
          (print indented-code))
        (when *modify*
          (write-file fpath indented-code)
          ;(write-file "temp.yasi" indented-code)
          ;(delete-file fpath)
          ;(rename-file "temp.yasi" fpath)
          )))))

(define (indent-file fpath)
  (letn ((fname (real-path fpath))
         (code (read-file! (or fname (filename-from-path fpath))))
         (indent-result (indent-code code fname))
         (backup-dir (or (get-backup-directory (main-args) fname) (real-path "."))))
    (after-indentation indent-result)
    (when (and *backup* (not backup-dir))
      (backup-source-file! fname backup-dir true))
    (when (and backup-dir *backup*)
      (backup-source-file! fname backup-dir true))))

;; newlisp includes everything typed in the command line in $main-args
;; ("newlisp" "yasi.lsp")
(when (> (length $main-args) 3)
  (indent-file ($main-args 2))
  (exit))

(when (= (length $main-args) 2)
  (print *help*)
  (exit))


