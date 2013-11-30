;; -*- emacs-lisp -*-

;; -------------------- mode-settings --------------------

(autoload 'c-mode "cc-mode" "C Editing Mode" t)
(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'bison-mode "bison-mode" "Major mode for editing yacc files." t)
(autoload 'flex-mode "flex-mode" "Major mode for editing lex files." t)
(autoload 'make-regexp "make-regexp" "Return a regexp to match a string item in STRINGS." t)
(autoload 'make-regexps "make-regexp" "Return a regexp to REGEXPS." t)
(autoload 'git-status "git" "git status mode." t)

(autoload 'insert-c++-seperator-line "e-seperators" nil t)
(autoload 'insert-c-seperator-line "e-seperators" nil t)
(autoload 'insert-elisp-seperator-line "e-seperators" nil t)
(autoload 'insert-script-seperator-line "e-seperators" nil t)
(autoload 'insert-c-section-header "e-seperators" nil t)
(autoload 'insert-c++-section-header "e-seperators" nil t)
(autoload 'insert-elisp-section-header "e-seperators" nil t)
(autoload 'insert-script-section-header "e-seperators" nil t)
(autoload 'insert-c++-big-header "e-seperators" nil t)
(autoload 'insert-elisp-big-header "e-seperators" nil t)
(autoload 'insert-script-big-header "e-seperators" nil t)

(setq auto-mode-alist (append '(("\\.[Cc][Xx][Xx]$" . c++-mode)
                                ("\\.[Cc][Pp][Pp]$" . c++-mode)
                                ("\\.[Hh][Xx][Xx]$" . c++-mode)
                                ("\\.[Tt][Cc][Cc]$" . c++-mode)
                                ("\\.mak$" . makefile-mode)
                                ("\\.mk$" . makefile-mode)
                                ("\\.conf$" . conf-mode)
                                ("\\.y$" . bison-mode)
                                ("\\.yy$" . bison-mode)
                                ("\\.l$" . flex-mode)
                                ("\\.ll$" . flex-mode)
                                ("\\.patch$" . diff-mode)
                                ("\\.diff$"  . diff-mode)
                                ("/diff\\'"  . diff-mode)
                                ("\\.org$" . org-mode))
                              auto-mode-alist))

;; load my cc-mode
(require 'cc-mode-settings)

;; load my latex-mode
(require 'latex-mode-settings)

(provide 'mode-settings)
