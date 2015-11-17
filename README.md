[![Build Status](https://travis-ci.org/nkmathew/yasi-sexp-indenter.svg?branch=master)](https://travis-ci.org/nkmathew/yasi-sexp-indenter)

##yasi- yet another s-expression indenter
yasi is a dialect-aware s-expression indenter that tries to improve  
on [Dorai's indenter][0] and *Vim's* built in indenter. It can handle  
*Common Lisp*, *Clojure*, *Scheme* and *newLISP* code and their  
unique syntaxes.

It's a batch mode indenter inspired by Dorai's [lispindent.lisp][1]
that was written first in *Python* and later translated to *newLISP*.  

Its style of indentation is  very close to that of *Dorai's* *lispindent.lisp*  
and tries to follow *Dorai's* [style guidelines][0] where reasonable.
(Refer to file dorai-test.lisp)


###Features
*yasi's* indentation relies heavily on regular expressions that  
give it an edge over its counterpart *lispindent.lisp*. Its  
features include:  

+ Ability to compact code that has extra whitespace between forms.  
  e.g. Code like `(if  (= lisp Clojure ) 'Yes 'No )` is compacted  
  to `(if (= lisp Clojure) 'Yes 'No)`  

+ Supports 4 dialects of the *Lisp* family giving you the correct indentation  
  of a form according to the dialect's syntax. e.g. The `do` keyword that  
  is used for looping in *Lisp* and sequential execution in *Clojure*.  
  The keyword should look like this in the two dialects:  

```lisp
;; In Common Lisp
(do ((j 0 (+ j 1)))
    (nil)                       ;Do forever.
  (format t "~%Input ~D:" j)
  (let ((item (read)))
    (if (null item) (return)   ;Process items until NIL seen.
      (format t "~&Output ~D: ~S" j item))))

;; In Clojure
(do
 (println "LOG: Computing...")
 (+ 1 1))
```

+ *yasi* also issues warnings that would become syntax errors if you  
  ran the code. e.g. If you forgot to close a string or a comment.  

+ Correct indentation of user defined macros. If the `defmacro` keyword is  
  found, the following macro name is added to the list of keywords with indentation  
  level of 2.  

```lisp
;; from quicklisp.lisp
(defmacro with-connection ((connection host port) &body body)
  `(call-with-connection ,host ,port (lambda (,connection) ,@body)))

(with-connection (connection (hostname connect-url) (port connect-url))
  (let ((cbuf (make-instance 'cbuf :connection connection))
        (request (request-buffer "GET" url)))
    (format stream "; ~$KB~%" (/ size 1024))
    (format stream "; Unknown size~%")))
```

+ It indents `flets` and `labels` the right way, using a hack here and there.  

+ Overkill feature: You get to know the last time you indented the
  file.

### Command Line Arguments
     _________________________________________________________________________________________________________________
    |    Usage: newlisp yasi.lsp [[<file>] [--backup-dir<directory>] [--no-compact] [--no-backup] [--no-warning]      |
    |                   [--clojure] [--lisp] [--scheme] [--default-indent <num>]]                                     |
    |            -nb,    --no-backup     # Don't create a backup file even if --backup-dir is specified               |
    |            -nc,    --no-compact    # Try to preserve the structure of the file.                                 |
    |            -nw,    --no-warning    # Don't issue warnings                                                       |
    |            -ne,    --no-exit       # Instructs the program not to exit when a warning is raised. True by default|
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

### What yasi does not handle
There are some syntaxes used in some dialects of scheme that I didn't feel like  
including because they are not that common. An example is *MzScheme* and  
*Gauche's* use of `#//` or `#[]` for regular expressions. I just saw it as too  
much an effort for something I would not use that often.

### Customization
*yasi* is customized differently from of Dorai's indenter. Instead of you  
storing extra keywords in a *.lispwords* file in your home directory, you are  
advised  to edit the list of keywords in the source file so that you have only  
one file. For example, if you find that `multiple-value-bind` should have all  
of its subforms indented uniformly like so:  

```lisp
(multiple-value-bind (f r)
  (floor 130 11)
  (list f r))
```

Instead of:  

```lisp
(multiple-value-bind (f r)
    (floor 130 11)
  (list f r))
```

You go to *line 424(yasi.py)*:  

```python
if DIALECT == 'Common Lisp': # Lisp
    TWO_SPACE_INDENTERS = LISP_KEYWORDS
    IF_LIKE += ['multiple-value-bind', 'destructuring-bind', 'do', 'do*']
```
and simply remove `multiple-value-bind` from the list.

#### About the default indent

The *--default-indent* comes in in expressions whose subforms usually  
start in the subsequent lines. Like a `cond` expression:  

```lisp
(cond
 ((> this that) 'Yes)
 ((= those these) 'No))
```

According to Dorai's [guidelines][0]
the above indentation is correct. However,  
some people may prefer the test expressions to be two spaces past the bracket,  
like this:  

```lisp
(cond
  ((> newLISP CL) 'Yes)
  ((= Clojure Lisp) 'No))
```

This is *Vim's* default style of indentation.  
That option enables you to specify the amount you want, for example to achieve  
the style above, you pass the parameter like so:

    yasi.py test.lisp --lisp --default-indent 2

----------

#### Changes made to Dorai's Indenter
I made a couple of modifications to *lispindent.lisp* and renamed it to  
*lispindent2.lisp*. It contains some of *yasi's* features. The changes  
include:  

+ Added comments for some sections of the program that took me time to grok.   

+ It can now indent files from the command line without the need to redirect  
  file contents to the program.

+ *lispindent2.lisp* indents *Clojure's* *vectors* and *sets*  
  better, i.e with an indentation level of 1, without affecting  
  *lisp* or *scheme's indentation*. It uses the file's extension to determine  
  if it's looking at *Clojure* code.
  e.g.

```lisp
;; lispindent2.lisp's indentation
(print {define "The keyword does not affect indentation"
    })

;; lispindent.lisp's indentation
(print {define "The keyword does not affect indentation"
   })
```

+ *lispindent2.lisp* ignores any code in a multiline comment and won't  
  be affected by any unclosed brackets inside the comment like the original  
  version. Unfortunately, its method of detecting multiline comments is rather  
  naive and introduces a bug in the code. Refer to its issues below.  

* *lispindent2.lisp* writes files using *LF* line endings be default. It's less  
  irritating than *CRLF* endings which usually light up in an annoying way in *Vim*.  

#### lispindent2.lisp's Issues

I inadvertently added a bug in an attempt to prevent it from evaluating  
brackets inside multiline comments and symbols with whitespace. It uses the pipe  
character(|) to switch between `multiline-commentp=T` and `multiline-commentp=NIL`  
which means that if your pipes are unbalanced, you'll get wrong indentation, e.g.:  

```lisp
#|*******************************************************************|
 |   This is a multiline comment that will trip the indenter         |
 |   because the odd number of pipes will cause `multiline-commentp` |
 |   to be true after this comment. It means the rest of the code    |
 |   won't be indented because it thinks it's still in a comment.    |
          Total pipes=11(odd)
 |#
 (print (cons
    'Hello ;; This line and the one below won't change
    'World
        ))
```

I don't find this to be a major issue because multiline comments are rare,  
the common use case being to comment out some piece of code when debugging.  

*lispindent2.lisp* uses the *Lisp* reader function `read-from-string` to get  
lisp forms and atoms from the read string. The downside of this is that  
`read-from-string` will fail when the code in the string is 'malformed'.  
For example, if it finds that the dot operator used for consing in *Common Lisp*  
comes after the opening bracket, it will raise a fatal error. This  
means that any *Clojure* code that tries to use the dot operator to access a  
class method will not be indented because of the error. An example is this code:  

```lisp
(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))
```

*lispindent2.lisp* uses the `ignore-errors` macro as a workaround. Doing that means that it can't  
run in *GNU Common Lisp* because it doesn't have the macro.

#### lispindent2.lisp's command-line options
     ___________________________________________________________________________
    |   Usage:  lispindent2.lisp [[<file>] [--no-modify] [--no-output]]         |
    |           --no-output ;; Don't output the indented code, false by default |
    |           --no-modify ;; Don't modify the file, false by default          |
    +---------------------------------------------------------------------------+

[0]: http://ds26gte.github.io/scmindent/index.html
[1]: https://github.com/ds26gte/scmindent/blob/master/lispindent.lisp
