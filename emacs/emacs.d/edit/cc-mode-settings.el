;; -*- emacs-lisp -*-

;; -------------------- cc-mode-settings --------------------

(defconst wnzheng-c-style
  '((c-tab-always-indent                     . t)
    (c-hanging-braces-alist                  . ((defun-open after)
                                                (defun-close before after)
                                                (class-open after)
                                                (class-close before after)
                                                (namespace-open after)
                                                (inline-open after)
                                                (inline-close before after)
                                                (block-open after)
                                                (block-close . c-snug-do-while)
                                                (extern-lang-open after)
                                                (extern-lang-close after)
                                                (statement-case-open after)
                                                (substatement-open after)
                                                (brace-list-open)
                                                (brace-entry-open)
                                                (statement-cont)
                                                (module-open after)
                                                (composition-open after)
                                                (inexpr-class-open after)
                                                (inexpr-class-close before)))
    (c-cleanup-list                          . (brace-else-brace
                                                brace-elseif-brace
                                                brace-catch-brace
                                                empty-defun-braces
                                                defun-close-semi
                                                list-close-comma
                                                scope-operator))
    (c-hanging-colons-alist                  . ((case-label)
                                                (label after)
                                                (access-label after)
                                                (member-init-intro before)
                                                (inher-intro)))
    (c-hanging-semi&comma-criteria           . (c-semi&comma-no-newlines-for-oneline-inliners
                                                c-semi&comma-inside-parenlist
                                                c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p       . nil)
    (c-recognize-knr-p                       . nil)
    (c-enable-xemacs-performance-kludge-p    . t) ; speed up indentation in XEmacs
    (c-basic-offset                          . 4)
    (indent-tabs-mode                        . nil)
    (c-comment-only-line-offset              . 0)
    (comment-column                          . 120)
    (c-offsets-alist                         . ((func-decl-cont . ++)
                                                (member-init-intro . ++)
                                                (inher-intro . ++)
                                                (comment-intro . 0)
                                                (arglist-close . c-lineup-arglist)
                                                (topmost-intro . 0)
                                                (block-open . 0)
                                                (inline-open . 0)
                                                (substatement-open . 0)
                                                (label . /)
                                                (case-label . 0)
                                                (statement-case-open . +)
                                                (statement-case-intro . +) ; case w/o {
                                                (access-label . /)
                                                (innamespace . 0))))
  "wnzheng C/C++ Programming Style")

(c-add-style "wnzheng" wnzheng-c-style)
(setq c-default-style
      '((java-mode . "java") (awk-mode . "awk")))

;; reset c-mode
(defun reset-c-mode nil
  (interactive)
  (if (eq major-mode 'c-mode)
      (progn (text-mode) (c-mode))))

(add-hook 'c-initialization-hook
          '(lambda nil
             (define-key c-mode-base-map "\C-m" 'c-context-line-break)
             (define-key c-mode-base-map [ret] 'c-newline-and-indent)
             (define-key c-mode-base-map (kbd "C-c C-d") 'c-electric-delete-forward)
             (define-key c-mode-base-map (kbd "C-c C-<DEL>") 'c-electric-backspace)
             (define-key c-mode-base-map (kbd "C-d") 'cut-region)
             (define-key c-mode-base-map [f10] 'reset-c-mode)))

(add-hook 'c-mode-common-hook
          '(lambda nil
             (c-toggle-hungry-state)
             (c-set-style "wnzheng")
             (which-func-mode t)
             (setq gdb-many-window t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((c-set-style . "BSD")))))

(provide 'cc-mode-settings)
