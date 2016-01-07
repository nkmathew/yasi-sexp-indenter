;23456789 123456789 123456789 123456789 123456789 123456789 123456789

                 (defun previous-user-buffer
                   () "Switch to the next user buffer in cyclic order."
                   (interactive)
                   (previous-buffer)
                   (let
                     ((i 0))
                     (while
                      (and
                       (string-match "^*" (buffer-name))
                       (< i 10))
                      (setq i (1+ i))
                      (previous-buffer))))

      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  ;; Use spaces, not tabs.
                  (setq indent-tabs-mode nil)
                  ;; Keep M-TAB for `completion-at-point'
                  (define-key flyspell-mode-map "\M-\t" nil)
                  ;; Pretty-print eval'd expressions.
                  (define-key emacs-lisp-mode-map
                    "\C-x\C-e" 'pp-eval-last-sexp)
                  ;; Recompile if .elc exists.
                  (add-hook (make-local-variable 'after-save-hook)
                            (lambda ()
                              (byte-force-recompile default-directory)))
                  (define-key emacs-lisp-mode-map
                    "\r" 'reindent-then-newline-and-indent)))

  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
														
      (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell
