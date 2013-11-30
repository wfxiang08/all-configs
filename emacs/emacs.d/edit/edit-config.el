;; -*- emacs-lisp -*-

;; -------------------- edit-config --------------------

;; set default encoding
(setq-default buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; set default tab-width
(setq-default tab-width 4)
(add-hook 'text-mode-hook
          (lambda () (setq indent-line-function 'insert-tab)))

;; insert space for indentation rather than tab characters
(setq-default indent-tabs-mode nil)
(progn (setq tab-stop-list nil)
       (let ((x 128))
         (while (> x 0)
           (setq tab-stop-list (cons (* x 4) tab-stop-list))
           (setq x (- x 1)))))

;; disable backup files
(setq make-backup-files nil)
(setq-default make-backup-files nil)

;; set kill-ring max
(setq kill-ring-max 1024)

;; show paren
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 120 columns default
(setq set-fill-column 120)
(setq default-fill-column 120)
(setq fill-column 120)

;; define goto paren
(defun goto-paren nil
  (interactive)
  (cond
   ((looking-at "[ \t]*[[\"({]") (forward-sexp) (backward-char))
   ((or (looking-at "[]\")}]") (looking-back "[]\")}][ \t]*")) (if (< (point) (point-max)) (forward-char)) (backward-sexp))
   (t (message "Cannot find matching brackets."))))

;; bind a shortcut key
(global-set-key (kbd "C-5") 'goto-paren)

;; define copy-line
(defun copy-region nil
  (interactive)
  (if mark-active
      (call-interactively 'kill-ring-save)
    (let ((pos (point)))
      (beginning-of-line)
      (let ((begin (point)))
        (forward-line 1)
        (kill-ring-save begin (point)))
      (goto-char pos)
      (message "copy a single line"))))

(global-set-key (kbd "M-w") 'copy-region)

;; define cut-region
(defun cut-region nil
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(global-set-key (kbd "C-d") 'cut-region)

;; define insert-line-after
(defun insert-line-after nil
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'insert-line-after)

;; define insert-line-before
(defun insert-line-before nil
  (interactive)
  (push-mark)
  (let* ((ipt (progn (back-to-indentation) (point)))
         (bol (progn (move-beginning-of-line 1) (point)))
         (indent (buffer-substring bol ipt)))
    (newline)
    (previous-line)
    (insert indent)))

;; set scroll buffer
(global-set-key (kbd "C-f") 'scroll-up)
(global-set-key (kbd "C-b") 'scroll-down)

;; forward/backward word
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-<right>") 'forward-word)

;; set smart home/end
(defun smart-home nil
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (skip-syntax-forward " \t" (line-end-position))
    (if (= pos (point))
        (beginning-of-line))))

(defun smart-end nil
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (skip-syntax-backward " \t" (line-beginning-position))
    (if (= pos (point))
        (end-of-line))))

(global-set-key (kbd "C-a")    'smart-home)
(global-set-key (kbd "C-e")    'smart-end)
(global-set-key (kbd "<home>") 'smart-home)
(global-set-key (kbd "<end>")  'smart-end)

(global-set-key [C-M-home] 'beginning-of-buffer)
(global-set-key [C-M-end]  'end-of-buffer)

;; set smart delete word
(defun smart-delete-word nil
  (interactive "*")
  (let ((pos (point)))
    (skip-chars-forward " \t")
    (if (< pos (point))
        (kill-region pos (point))
      (if (eq (point) (line-end-position))
          (delete-forward-char 1)
        (delete-region (point) (progn (forward-word 1) (point)))))))

(defun smart-backward-delete-word nil
  (interactive "*")
  (let ((pos (point)))
    (skip-chars-backward " \t")
    (if (> pos (point))
        (kill-region (point) pos)
      (if (eq (point) (line-beginning-position))
          (backward-delete-char-untabify 1)
        (delete-region (point) (progn (forward-word -1) (point)))))))

(global-set-key (kbd "M-d") 'smart-delete-word)
(global-set-key [C-kp-delete] 'smart-delete-word)
(global-set-key [M-backspace] 'smart-backward-delete-word)
(global-set-key [C-backspace] 'smart-backward-delete-word)

;; goto line & mark command
(define-key ctl-c-map (kbd "C-g") 'goto-line)
(define-key ctl-c-map "v" 'set-mark-command)
(define-key ctl-c-map "o" 'occur)
(define-key ctl-c-map "r" 'revert-buffer)

;; comment & uncomment
(defun smart-comment-uncomment nil
  (interactive)
  (if mark-active
      (call-interactively 'comment-or-uncomment-region)
    (let ((pos (point)))
      (beginning-of-line)
      (set-mark-command nil)
      (end-of-line)
      (call-interactively 'comment-or-uncomment-region)
      (goto-char pos))))

(define-key ctl-c-map "/" 'smart-comment-uncomment)

;; enable redo+ mode and bind undo/redo a shortcut key
(require 'redo+)
(global-set-key [f7] 'redo)
(global-set-key [f8] 'undo)

;; set for [delete] [backspace] [kp-delete]
;; set for [home] [end]
;; for mac os x
(if (eq system-type 'darwin)
    (progn (normal-erase-is-backspace-mode 0)
           (global-set-key [kp-delete] 'delete-forward-char)
           (global-set-key [delete] 'delete-forward-char)))
(global-set-key (kbd "M-[ h") 'smart-home)
(global-set-key (kbd "M-[ f") 'smart-end)

;; set for indent/unindent-tablist region
(defun indent-tablist ()
  "Indent line or region using tab stops."
  (interactive)
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-indentation) (car tabs)))
      (setq tabs (cdr tabs)))
    (let ((tab (if tabs (car tabs) (+ (current-indentation) tab-width))))
      (if (use-region-p)
          (indent-rigidly (region-beginning) (region-end)
                          (- tab (current-indentation)))
        (indent-rigidly (line-beginning-position) (line-end-position)
                        (- tab (current-indentation)))))))
(defun unindent-tablist ()
  "Unindent line or region using tab stops."
  (interactive)
  (let ((tabs (reverse tab-stop-list)))
    (while (and tabs (<= (current-indentation) (car tabs)))
      (setq tabs (cdr tabs)))
    (let ((tab (if tabs (car tabs) 0)))
      (if (use-region-p)
          (indent-rigidly (region-beginning) (region-end)
                          (- tab (current-indentation)))
        (indent-rigidly (line-beginning-position) (line-end-position)
                        (- tab (current-indentation)))))))
(global-set-key (kbd "M-TAB") 'indent-tablist)
(global-set-key [backtab] 'unindent-tablist)

;; mode-settings
(require 'mode-settings)

;; desktop-settings
(require 'desktop-settings)

;; enable taglist
(require 'taglist)

(provide 'edit-config)
