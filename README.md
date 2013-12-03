##Yasi- yet another s-expression indenter
Yasi is a dialect aware s-expression indenter that tries to improve  
on
[Dorai's indenter](http://www.ccs.neu.edu/home/dorai/scmindent/)
and *Vim's* built in indenter. It can handle  
*Common Lisp*, *Clojure*, *Scheme* and *newLISP* code and their  
unique syntaxes.

It's a batch mode indenter inspired by Dorai's *lispindent.lisp* that was  
written first in *Python* and later translated to *newLISP*.  

It's style of indentation is  very close to that of *Dorai's* *lispindent.lisp*  
and tries to follow all of *Dorai's* [rules](http://www.ccs.neu.edu/home/dorai/scmindent/)  


###Features
*Yasi's* indentation relies heavily on regular expressions that  
give it an edge over it's counterpart *lispindent.lisp*. It's  
features include:  

+ Ability to compact code that has extra whitespace between forms.  
  e.g. Code like `(if  (= lisp Clojure ) 'Yes 'No )` is compacted  
  to `(if (= lisp Clojure) 'Yes 'No)`  
+ Supports a couple of *Lisp* dialects so that any keywords that exist  
  in more than one dialect, are indented in according to its meaning  
  in that dialect. A keyword like `do` which is used for sequential  
  execution in `Clojure` won't be indented the same way in `Lisp` which uses  
  `do` as an iteration construct.  
        
        ;; Iteration in Lisp and Scheme, note the loop terminator
        ;; has a different level from that of the body.
        (do ((i 0 (+ 1 i)))
            ((= i (string-length str)) 'done)
          (display (format 
                    "Offset: ~s, Char: ~s~%" (string-ref str i) i))))
    
        ;; If it were not for per dialect keywords, the first form 
        ;; would be indented 2 spaces to the left being confused with
        ;; Lisp/Scheme's iteration construct.
        (do
          (print "Clojure just had to be different")
          (print "It's a Lisp that is breaking loose")
         )

+ *Yasi* also issues warnings that would become syntax errors if you  
  ran the code. e.g. If you forgot to close a string or a comment.  

+ Correct indentation of user defined macros. If *Yasi* finds a `defmacro`  
  keyword, it adds the macro name to the list of keywords with indentation level  
  of 2.

        ;; from quick.lisp
        (defmacro with-connection ((connection host port) &body body)
          `(call-with-connection ,host ,port (lambda (,connection) ,@body)))

        (with-connection (connection (hostname connect-url) (port connect-url))
          (let ((cbuf (make-instance 'cbuf :connection connection))
                (request (request-buffer "GET" url)))
            (format stream "; ~$KB~%" (/ size 1024))
            (format stream "; Unknown size~%")))

+ It indents `flets` and `labels` the right way, using a hack here and there.  

+ It doesn't mess with `newLISP`'s multiline strings({})

+ Overkill feature: You get to know the last time you indented the
  file.

###Command Line Arguments
     _________________________________________________________________________________________________________________
    |    Usage: yasi.py [[<file>] [--backup-dir<directory>] [--no-compact] [--no-backup] [--no-warning]               |
    |                   [--clojure] [--lisp] [--scheme]]                                                              |
    |            -nb,    --no-backup     # Don't create a backup file even if --backup-dir is specified               |
    |            -nc,    --no-compact    # Try to preserve the structure of the file.                                 |
    |            -nw,    --no-warning    # Don't warn about unmatched brackets or bad commenting.                     |
    |            -ne,    --no-exit       # Instructs the program to exit when a warning is raised. True by default    |
    |            -uni,   --uniform       # Dicatates whether the if-clause and else-clause of an if-like block should | 
    |                              have the same indent level. False by default                                       |
    |            --backup-dir    # The directory where the backup file is to be written                               |
    |            --clojure       # Use Clojure keywords                                                               |
    |            --lisp          # Use Lisp keywords                                                                  |
    |            --scheme        # Use Scheme keywords                                                                |
    |            --newlisp       # Use newLISP keywords                                                               |
    |            --no-output     # Suppress output of the indented code                                               |
    |            --no-modify     # Don't modify the file                                                              |
    +-----------------------------------------------------------------------------------------------------------------+


###What Yasi does not handle
There are some syntaxes used in some dialects of scheme that I didn't feel like  
including because they are not that common. An example is *MzScheme* and  
*Gauche's* use of `#//` or `#[]` for regular expressions. I just saw it as too  
much an effort for something I would use rarely if ever at all.

###Customization
Unlike *lispindent.lisp* that requires you to put keywords in a *.lispwords*  
file in your home directory, *yasi* keeps an internal list of those keywords.  
Any additional keyword is placed in the appropriate dialect's list so that  
you have only one file.

####About the default indent

The *--default-indent* comes in in expressions whose first arguments usually  
start at the next line. Like a `cond` expression:  

    (cond
     ((> this that) 'Yes)
     ((= those these) 'No))

According to the normal [rules](http://www.ccs.neu.edu/home/dorai/scmindent/)  
the above indentation is correct. However, some people that prefer  
the test expressions to be two spaces past the bracket, like this:  

    (cond
      ((> newLISP CL) 'Yes)
      ((= Clojure Lisp) 'No))

It makes the body stand out well enough to be easily noticeable; *Vim* does this.  
That option enables you to specify the amount you want, for example to achieve  
the style above, you pass the parameter like so:

    yasi.py test.lisp --lisp --default-indent 2

####Testing
I used a rather unorthodox method of testing the three programs(*yasi.py*,  
*original_yasi.py*, *yasi.lsp*). I figured, since the programs follow the  
same logic, they should give the same result for any file with the same  
parameters passed to them.  
This gurrantees that the programs were translated correctly but doesn't mean  
that the way they are indenting is the right way. You should get similar results  
when you run a command like this:  

    forfiles /c "cmd /c newlisp ..\yasi.lsp @file --scheme -nb -no -ne"
    forfiles /c "cmd /c python ..\yasi.py @file --scheme -nb -no -ne"
    forfiles /c "cmd /c python ..\original-yasi.py @file --scheme -nb -no -ne"


----------

####Changes made to Dorai's Indenter
I made a couple of modifications to *lispindent.lisp* and renamed it to  
*lispindent2.lisp*. I used *lispindent.lisp's*  way of indenting forms as the  
'right' way when creating *Yasi* and  added some of *Yasi's* features to it  
later. Some of the modifications include:    

+ Added a bunch of comments that'll make understanding the code easier.  
  The original one was quite hard to grok.  

+ I turned into a batch-mode indenter, which was quite easy really since *Lisp*  
  uses streams. You simply open a file and pass the file descriptor to most  
  of the i/o functions like `terpri` and `write`.

+ *lispindent2.lisp* indents *Clojure's* *vectors* and *hash sets*  
  better, i.e with an indentation level of 1. This doesn't affect  
  *lisp* or *scheme indentation*.  
  e.g

        ;; lispindent2.lisp's indentation
        (print {define "The keyword does not affect indentation"
                })

        ;; lispindent.lisp's indentation
        (print {define "The keyword does not affect indentation"
               })

+ *lispindent2.lisp* ignores any code in a multiline comment and won't  
  be affected by any unclosed brackets inside the comment like the original  
  version. Unfortunately, its method of detecting multiline comments is rather  
  naive and introduces a bug in the code. Refer to its issues below.  

* *lispindent2.lisp* writes files using *LF* line endings be default. It's less  
  irritating than *CRLF* endings which usually light up in an annoying way in *Vim*.  

####lispindent2.lisp's Issues

I inadvertently added a bug in an attempt to prevent it from evaluating  
brackets inside multiline comments and symbols with whitespace. It uses the pipe  
character(|) to switch between `multiline-commentp=true` and `multiline-commentp=false`  
which means that if your pipes are unbalanced, you'll get wrong indentation, e.g.:  

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
I don't find this to be a major issue because multiline comments are rare,  
the common use case being to comment out some piece of code when debugging.  

*lispindent2.lisp* uses the *Lisp* reader function `read-from-string` to get  
lisp forms and atoms from the read string. The downside of this is that  
`read-from-string` will fail when the code in the string is 'malformed'.  
For example, if it finds that the dot operator used for consing in *Common Lisp*  
comes after the opening bracket, it will stop reading from the string. This  
means that any *Clojure* code that tries to use the dot operator to access a  
class method will not be indented because of the error. An example is this code:  

    (defmacro chain
      ([x form] `(. ~x ~form))
      ([x form & more] `(chain (. ~x ~form) ~@more)))

It can be solved with the `ignore-errors` macro but that means that it can't  
run with *GNU Common Lisp* because it doesn't have the macro.

####lispindent2.lisp's command-line options
     ___________________________________________________________________________
    |   Usage:  lispindent2.lisp [[<file>] [--no-modify] [--no-output]]         |
    |           --no-output ;; Don't output the indented code, false by default |
    |           --no-modify ;; Don't modify the file, false by default          |
    +---------------------------------------------------------------------------+

####About yasi.lsp
*yasi.lsp* is the *newLISP* translation of the python version. I translated  
almost every method with little modification from Python to newLISP. *yasi.lsp*  
should therefore run almost 100% like *yasi.py*  


