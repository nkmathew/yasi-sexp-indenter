(defun the-answer-2 (library)
  (multiple-value-bind (answer sure-p)
      (get-answer library)
    (if (not sure-p)
        "I don't know"
      (format nil "The answer is ~A" answer))))
