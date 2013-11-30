;; -*- emacs-lisp -*-

;; -------------------- dired-x-settings --------------------

;; load dired-x mode
(require 'dired-x)
(require 'dired-sort-map)

(add-hook 'dired-mode-hook (lambda () (load "dired-x")
                             (dired-omit-mode nil)))

(require 'dired-isearch)

(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
(define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)

(define-key global-map (kbd "C-x 4 d") 'dired-jump)
(define-key global-map (kbd "C-x 4 C-d") 'dired-jump-other-window)

(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (make-local-variable  'dired-sort-map)
            (setq dired-sort-map (make-sparse-keymap))
            (define-key dired-mode-map "s" dired-sort-map)
            (define-key dired-sort-map "s"
              '(lambda () "sort by Size"
                 (interactive)
                 (dired-sort-other (concat dired-listing-switches "S"))))
            (define-key dired-sort-map "x"
              '(lambda () "sort by eXtension"
                 (interactive)
                 (dired-sort-other (concat dired-listing-switches "X"))))
            (define-key dired-sort-map "t"
              '(lambda () "sort by Time"
                 (interactive)
                 (dired-sort-other (concat dired-listing-switches "t"))))
            (define-key dired-sort-map " "
              '(lambda () "sort by Name"
                 (interactive)
                 (dired-sort-other (concat dired-listing-switches ""))))))

(defun directory-first-dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'directory-first-dired-sort)

;; omit all hidden files which starts with '.' & omit all auto-save files and lock files
(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^\\.?#\\|^#\\|^\\..*")
            (dired-omit-mode 1)))

(provide 'dired-x-settings)
