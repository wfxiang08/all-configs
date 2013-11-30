;; -*- emacs-lisp -*-

;; -------------------- shell-settings --------------------

;; set shell mode encode
(add-hook 'shell-mode-hook
          '(lambda() (set-buffer-process-coding-system 'utf-8 'utf-8)))

(add-hook 'comint-output-filter-functions
          'cominit-watch-for-password-prompt)

(provide 'shell-settings)
