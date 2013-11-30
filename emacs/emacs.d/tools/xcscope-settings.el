;; -*- emacs-lisp -*-

;; -------------------- xcscope-settings --------------------

;; load xcscope
(require 'xcscope)
(setq cscope-do-not-update-database t)
(define-key cscope-list-entry-keymap (kbd "<RET>")
  '(lambda () (interactive) (cscope-select-entry-one-window)))
(define-key cscope-list-entry-keymap "o"
  '(lambda () (interactive) (cscope-select-entry-other-window)))

(require 'postack)

(add-hook 'c-mode-common-hook
          '(lambda nil
             (progn (define-key ctl-c-map "]" 'cscope-find-this-symbol)
                    (define-key ctl-c-map "t" 'postack-pop))))

(provide 'xcscope-settings)
