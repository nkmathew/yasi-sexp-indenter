;; Demonstrates how Emacs indents forms according to the below url
;; http://www.emacswiki.org/emacs/IndentingLisp

(lambda (foo)
  "foo"
  "bar")

(while (foo)
  "foo"
  "bar")

(if (foo)
    "foo"
  "bar")

(lambda
  (foo)
  "foo"
  "bar")

(while
  (foo)
  "foo"
  "bar")

(if
    (foo)
  "foo"
  "bar")
