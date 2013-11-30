;; -*- emacs-lisp -*-

;; -------------------- config --------------------

(setq load-path (append (list (expand-file-name "~/.emacs.d/lisp")
                              (expand-file-name "~/.emacs.d/edit")
                              (expand-file-name "~/.emacs.d/view")
                              (expand-file-name "~/.emacs.d/tools"))
                        load-path))

;; disable version control (T.T~ sshfs)
(setq vc-handled-backends nil)

;; require 'require-maybe'
(require 'require-maybe)

;; require 'eval-after-load' to speedup
;;(require 'eval-after-load)

;; define key-maps
(define-prefix-command 'ctl-c-map)
(global-set-key (kbd "C-c") 'ctl-c-map)

(global-unset-key (kbd "C-w"))
(define-prefix-command 'ctl-w-map)
(global-set-key (kbd "C-w") 'ctl-w-map)

;; delete the highlight region
(delete-selection-mode 1)

;; set syntax highlight
(global-font-lock-mode t)

;; trace end-of-line (set trace-eol to a non-nil value)
(setq trace-eol t)

;; use zsh as the default shell
(setq explicit-shell-file-name "zsh")

;; enable clipboard
(setq x-select-enable-clipboard t)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; set kill query
(setq kill-emacs-query-functions
      '(lambda()
         (y-or-n-p "Do you really want to quit?")))

;; set default mode & disable the startup messages
(setq-default major-mode 'text-mode)
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)

;; disable warning bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; enable -diff: usage, emacs -diff file1 file2
(defun command-line-diff(switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;; user's information
(setq user-full-name "Naizheng Wang")
(setq user-mail-address "wnzheng@gmail.com")

;; -------------------- require other configs --------------------
(require 'edit-config)
(require 'view-config)
(require 'tools-config)

;; provider
(provide 'emacs-config)
