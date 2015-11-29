;; Problem: The "with" in the file handle name is influencing the indentation
(with-open-file
  (file-with-code *file-name*
                  :direction :input))

(with-open-file (file-with-code "indented-file.lisp"
                                :direction :output :external-format :unix))
