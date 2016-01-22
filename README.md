[![Build Status][2]][3]
[![Latest tag][4]][5]
[![pypi downloads][6]][7]

## yasi - yet another s-expression indenter

- [Introduction](#introduction)
- [Installation](#installation)
- [Features](#features)
- [Command Line Arguments](#command-line-arguments)
- [Hanging Indentation](#hanging-indentation)
- [Customization](#customization)
- [About the default indent](#about-the-default-indent)
- [What yasi does not handle](#what-yasi-does-not-handle)
- [Modifications to lispindent](#modifications-to-lispindent)
- [Editor Integration](#editor-integration)
- [Lispindent2 Issues](#lispindent2-issues)
- [lispindent2 Command Line Options](#lispindent2-command-line-options)

### Introduction
yasi is a dialect-aware s-expression indenter that tries to improve on [Dorai's
indenter][0] and *Vim's* built in indenter. It can handle *Common Lisp*, *Clojure*,
*Scheme* and *newLISP* code and their unique syntaxes.

It's mainly a batch mode indenter inspired by Dorai's [lispindent.lisp][1]
that was written first in *Python* and later translated to *newLISP*.  

Its style of indentation is very close to that of *lispindent.lisp* and tries to
follow [these style guidelines][0] where reasonable.

It should find most use with programmers who use any other editor other than Emacs
which provides excellent indentation for lisp-like forms and s-expressions out of
the box.

I made this because there weren't any good enough tools out there that could indent
the code I would copy/paste and run from tutorials when I was starting out with Lisp.

### Installation
From pypi:

    pip install --upgrade yasi

### Features
*yasi's* indentation relies heavily on regular expressions that give it an edge
over its counterpart *lispindent.lisp*. Its features include:

+ Support for the different mainstream Lisps out there giving you the correct
  indentation of a form according to the dialect's syntax/semantic. e.g.
  The `do` keyword which is a looping construct in *Common Lisp* and sequential
  execution in *Clojure*. The keyword should look like this in the two dialects:

```lisp
;; In Common Lisp
(do ((j 0 (+ j 1)))
    (nil)                       ;Do forever.
  (format t "~%Input ~D:" j)
  (let ((item (read)))
    (if (null item) (return)   ;Process items until NIL seen.
      (format t "~&Output ~D: ~S" j item))))
```

```Clojure
;; In Clojure
(do
 (println "LOG: Computing...")
 (+ 1 1))
```

+ Ability to trim extraneous whitespace and compact code

+ Issues warnings for possible errors in code like unmatched brackets and unclosed
  strings

+ Correct indentation of user defined macros

+ Supports additional keywords through a config file in the current or home
  directory

+ Correct indentation of `flets` and `labels`, something that doesn't come out of
  the box even in Emacs

+ Indentation from standard input

+ The python version can output a unified diff between the initial and indented code

### Command Line Arguments

    usage: yasi [-h] [-nc] [-nb] [-nm] [--diff] [-nw] [-nr] [--no-output] [-c]
                [-ne] [-o OUTPUT_FILE] [--tab TAB_SIZE] [--dialect DIALECT] [-v]
                [-suffix BACKUP_SUFFIX] [-bd BACKUP_DIR] [-is INDENT_SIZE]
                [-di DEFAULT_INDENT] [-ic] [-uni]
                [files [files ...]]

    Dialect-aware s-expression indenter

    positional arguments:
      files                 List of files to be indented. Will indent from
                            standard input if no files are specified

    optional arguments:
      -h, --help            show this help message and exit
      -nc, --no-compact, --nc
                            Do not compact the code, just indent
      -nb, --no-backup, --nb
                            Do not create a backup file even if --backup-dir is
                            specified
      -nm, --no-modify, --nm
                            Do not modify the file
      --diff, -diff         Prints unified diff of the initial and final result
      -nw, --no-warning, --nw
                            Do not display warnings
      -nr, --no-rc, --nr    Ignore any rc files in the current or home folder
      --no-output, -no-output
                            Suppress output of the indented code
      -c, --color, -color   Display diff text in color
      -ne, --no-exit, --ne  Instructs the program not to exit when a warning is
                            raised.
      -o OUTPUT_FILE        Path/name of output file
      --tab TAB_SIZE, -tab TAB_SIZE
                            Indent with tabs using the specified tabwidth. A tab
                            is assumed equal to 4 spaces by default when expanding
                            the tabs in the input file
      --dialect DIALECT, -dialect DIALECT
                            Use Scheme keywords
      -v, --version         Prints script version
      -suffix BACKUP_SUFFIX, --suffix BACKUP_SUFFIX
                            Backup file suffix
      -bd BACKUP_DIR, --backup-dir BACKUP_DIR, --bd BACKUP_DIR, -backup-dir BACKUP_DIR
                            The directory where the backup file is to be written
      -is INDENT_SIZE, --indent-size INDENT_SIZE, --is INDENT_SIZE
                            The number of spaces per indent
      -di DEFAULT_INDENT, --default-indent DEFAULT_INDENT, --di DEFAULT_INDENT
                            The indent level to be used in case a function's
                            argument is in the next line. Vim uses 2, the most
                            common being 1.
      -ic, --indent-comments, --ic
                            If true, comment lines will be indented possibly
                            messing with any deliberate comment layout
      -uni, --uniform, -uniform, --uni
                            Dictates whether the if-clause and else-clause of an
                            if-likeblock should have the same indent level.

### Hanging Indentation
This is where the indented code block is not flush with the left margin. Lispindent
does this by default although differently to the way it's implemented in yasi.
The effect is obtained by passing **--no-compact** to the script.
Here's how hanging indentation in lispindent and yasi differs:

Initial code:

```lisp
;; Comment
                    (if (not (empty? macro-name))
                    (push (list macro-name KEYWORD1) keyword-lst)
                    nil)

        (if (not (empty? macro-name))
        (push (list macro-name KEYWORD1) keyword-lst)
        nil)

                (exit)
```

Calling yasi on the file with **--no-compact**:

```lisp
;; Comment
                    (if (not (empty? macro-name))
                        (push (list macro-name KEYWORD1) keyword-lst)
                      nil)

        (if (not (empty? macro-name))
            (push (list macro-name KEYWORD1) keyword-lst)
          nil)

                (exit)
```

How lispindent does it(the number of spaces at the start of first block defines
where the rest of the blocks in the file will start):

```lisp
;; Comment
                    (if (not (empty? macro-name))
                        (push (list macro-name KEYWORD1) keyword-lst)
                      nil)
                    
                    (if (not (empty? macro-name))
                        (push (list macro-name KEYWORD1) keyword-lst)
                      nil)
                    
                    (exit)

```

### Customization
Customization is done similarly to the way it's done in lispindent - keywords are
associated with numbers that determine the next line's indentation level.

The additional keywords are defined in a **.yasirc.json** file placed in the current
working directory of in the home folder. Should there be configuration files in both
directories the one in the current working directory will be preferred.

A typical config file looks like this:

```javascript
{
  "scheme": {
    "do": 2,
    "if": 2
  },
  "lisp": {
    "do": 2,
    "if": 2
  },
  "clojure": {
    "do": 2,
    "if": 2
  },
  "newlisp": {
    "do": 2,
    "if": 2
  }
}
```


The numbers are described below(assuming standard indentation size of 2 spaces):

  + **0** - Associating a keyword with zero turns it into a normal function i.e
    removes keywordness

```lisp
(do-the-boogie (= 12 44)
                (print "if clause")
                (print "else clause"))
```

  + **1** - Causes the subforms of the function to be indented uniformly by a unit
    indentation size(which can be changed)

```lisp
(do-the-boogie (= 12 44)
  (print "if clause")
  (print "else clause"))
```

  + **2** - Distinguishes the first subform by giving it a greater indentation than
    the rest of the subforms the same way the standard if expression is indented.
    The first subform has twice the indentation size as the rest.

```lisp
(do-the-boogie (= 12 44)
    (print "if clause")
  (print "else clause"))
```

  + **3** - Subforms will be indented uniformly by twice the indentation size

```lisp
(do-the-boogie (= 12 44)
    (print "if clause")
    (print "else clause"))
```

  + **4** - Indents by a unit like a 1-keyword but also its local functions

```lisp
(letfn [(six-times [y]
          (* (twice y) 3))
        (twice [x]
          (* x 2))]
  (println "Twice 15 =" (twice 15))
  (println "Six times 15 =" (six-times 15)))
```

  The standard indentation(assuming `letfn` is just another function) would be:

```lisp
(letfn [(six-times [y]
                    (* (twice y) 3))
        (twice [x]
                (* x 2))]
  (println "Twice 15 =" (twice 15))
  (println "Six times 15 =" (six-times 15)))
```


#### About the default indent

The *--default-indent* comes in in expressions whose subforms usually start in the
subsequent lines. Like in a `cond` expression:

```lisp
(cond
 ((> this that) 'Yes)
 ((= those these) 'No))
```

This above result would be the standard/expected indentation. However one might
prefer to have the subforms to start two spaces past the head of the expression like
this.

```lisp
(cond
  ((> newLISP CL) 'Yes)
  ((= Clojure Lisp) 'No))
```

This is *Vim's* default style of indentation.
That option enables you to specify the amount you want, for example to achieve the
style above, you pass the parameter like so:

```shell
    yasi.py test.lisp --lisp --default-indent 2
```

----------

### What yasi does not handle
There are some syntaxes used in some dialects of Scheme that didn't seem worth the
effort implementing. An example is *MzScheme* and *Gauche's* use of `#//` or `#[]`
for regular expressions.

#### Modifications to lispindent
I made a couple of modifications to *lispindent.lisp* and renamed it to
*lispindent2.lisp*. The changes include:

+ Added comments for some sections of the program that took me time to understand

+ It can now indent files from the command line without the need to redirect file
  contents to the program. The original one was purely intended to be used as a
  filter script indenting only from standard input.

+ *lispindent2.lisp* indents *Clojure's* *vectors* and *sets* better, i.e with an
 indentation level of 1, without affecting *Lisp's* or *Scheme's indentation*. It
  uses the file's extension to determine if it's looking at *Clojure* code.
  e.g.

```Clojure
;; lispindent2.lisp's indentation
(print {define "The keyword does not affect indentation"
    })
```

```Clojure
;; lispindent.lisp's indentation
(print {define "The keyword does not affect indentation"
   })
```

+ *lispindent2.lisp* ignores any code in a multiline comment and won't  be affected
  by any unclosed brackets inside the comment like the original version.
  Unfortunately, its method of detecting multiline comments is rather  naive and
  introduces a bug in the code. Refer to its issues below.

+ *lispindent2.lisp* writes files using *LF* line endings be default. It's less
  irritating than *CRLF* endings which usually light up in an annoying way in *Vim*.

#### Editor Integration
yasi's ability to format code from standard input makes it a suitable candidate for
the `equalprg` setting in Vim. Add this in your **.vimrc** and you're good to go.

```vim
au filetype clojure,lisp,scheme,newlisp setlocal equalprg=yasi.py\ --indent-comments
```

You can then indent a function/block by providing the motion after the `=` sign
e.g `=%`

You can also checkout these other projects for proper integration without invoking
it externally as a filter script for example:

  + Vim plugin: https://github.com/nkmathew/vim-newlisp-yasi
  + Sublime Text 2/3 plugin: https://github.com/nkmathew/sublime-yasi

#### lispindent2 Issues

I inadvertently added a bug in an attempt to prevent it from evaluating brackets
inside multiline comments in Common Lisp and symbols with whitespace in Scheme.

It uses the pipe character(|) to track whether the comment it's still in a multiline
comment meaning an odd number of pipes in a multiline comment will yield a wrong
indentation e.g.:

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

I don't find this to be a major issue because multiline comments are rarely used,
the common use case being to comment out regions of code when debugging.

*lispindent2.lisp* uses the *Lisp* reader function `read-from-string` to get lisp
forms and atoms from the read string.

The downside of this is that `read-from-string` will fail when the code in the
string is 'malformed'. For example, if it finds that the dot operator used for
consing in *Common Lisp* comes after the opening bracket, it will raise a fatal
error. This means that any *Clojure* code that tries to use the dot operator to
access a class method will not be indented because of the error. An example is this
code:

```Clojure
(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))
```

*lispindent2.lisp* uses the `ignore-errors` macro as a workaround. Doing that means
that it can't run in *GNU Common Lisp* because it doesn't have the macro.

#### lispindent2 Command Line Options
    +---------------------------------------------------------------------------+
    |   Usage:  lispindent2.lisp [[<file>] [--no-modify] [--no-output]]         |
    |           --no-output ;; Don't output the indented code, false by default |
    |           --no-modify ;; Don't modify the file, false by default          |
    +---------------------------------------------------------------------------+

[0]: https://github.com/ds26gte/scmindent
[1]: https://github.com/ds26gte/scmindent/blob/master/lispindent.lisp
[2]: https://travis-ci.org/nkmathew/yasi-sexp-indenter.svg?branch=master
[3]: https://travis-ci.org/nkmathew/yasi-sexp-indenter
[4]: https://img.shields.io/github/tag/nkmathew/yasi-sexp-indenter.svg
[5]: https://github.com/nkmathew/yasi-sexp-indenter/releases
[6]: https://img.shields.io/pypi/dm/yasi.svg
[7]: https://pypi.python.org/pypi/yasi
