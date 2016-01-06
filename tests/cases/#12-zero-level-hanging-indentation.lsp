        ;; Demonstrates hanging indentation(different zero levels)

)) ;; Comments should not affect the indentation but will still generate warnings

    (when (find func-name '("define-macro" "defmacro"))
    (letn ((end-of-space ((regex "^[ \t]*" substr) 2))
    (substr (slice substr end-of-space))
    (substr (strip
    (slice substr
    (or ((regex "[ \t]*" substr) 1) -1)) " \n\r\t"))
    (macro-name (slice substr 0 (or (find " " substr) -1))))
    (if (not (empty? macro-name))
    (push (list macro-name KEYWORD1) keyword-lst))))

              (when (find func-name '("define-macro" "defmacro"))
              (letn ((end-of-space ((regex "^[ \t]*" substr) 2))
              (substr (slice substr end-of-space))
              (substr (strip
              (slice substr
              (or ((regex "[ \t]*" substr) 1) -1)) " \n\r\t"))
              (macro-name (slice substr 0 (or (find " " substr) -1))))
              (if (not (empty? macro-name))
              (push (list macro-name KEYWORD1) keyword-lst))))


                                    (exit)
