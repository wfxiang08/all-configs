;; -*- emacs-lisp -*-

;; -------------------- help-settings --------------------

;; set help mode
(add-hook 'help-mode-hook
          '(lambda() (interactive)
             (define-key help-mode-map "b" 'help-go-back)
             (define-key help-mode-map "f" 'help-go-forward)))

(provide 'help-settings)
