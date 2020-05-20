(let* (
(case-fold-search nil)
(name "caillou")
)
(if (compare-strings name nil nil "CAILLOU" nil nil 'IGNORE-CASE)
(print "Fun cartoon)
(format t Not so fun cartoon")
)
)
