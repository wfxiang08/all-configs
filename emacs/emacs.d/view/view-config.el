;; -*- emacs-lisp -*-

;; -------------------- view-config --------------------

;; set font/color stuffs
(if (not (null window-system))
    (progn
      (require 'color-theme)
      (eval-after-load "color-theme"
        '(progn
           (color-theme-initialize)
           (color-theme-blackboard)))))

;; set default fonts
(if (not (null window-system))
    (progn (if (eq system-type 'darwin)
              (progn (set-default-font "Monaco 14")
                     (dolist (charset '(kana han symbol cjk-misc bopomofo))
                       (set-fontset-font (frame-parameter nil 'font)
                                         charset
                                         (font-spec :family "STSong" :size 16)))
                     (setq default-frame-alist
                           (append
                            '((font . "Monaco 14")) default-frame-alist))))))

;; set color for diff-mode
(custom-set-faces '(diff-added ((t (:foreground "#10a8d8"))))
                  '(diff-header ((((class color)) :foreground "green")))
                  '(diff-hunk-header ((t (:foreground "#c29621"))))
                  '(diff-file-header ((((class color)) :foreground "green")))
                  '(diff-function ((t (:foreground "#00bbdd"))))
                  '(diff-index ((t (:foreground "#c29621"))))
                  '(diff-removed ((t (:foreground "#de1923"))))
                  '(diff-indicator-added ((t :foreground "#10a8d8")))
                  '(diff-indicator-removed ((t :foreground "#de1923")))
                  '(diff-changed ((t :foreground "deep pink")))
                  '(diff-context ((t nil)))
                  '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042")))))

;; wheel for scale
(if (not (null window-system))
     (progn (if (eq system-type 'linux)
                (progn
                  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
                  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))
              (progn
                (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
                (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)))))

;; toggle show full name in status bar
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; enable number disable toolbar and menubar
(custom-set-variables '(column-number-mode t)
                      '(line-number-mode t)
                      '(tool-bar-mode nil)
                      '(menu-bar-mode nil)
                      '(scroll-bar-mode nil))

;; prevent newline at end of file
(setq require-final-newline nil)

;; set title bar
(if (not (null window-system))
    (require 'title-time))

(custom-set-variables '(display-time-mode t)
                      '(display-time-format "%Y-%m-%d %H:%M (%a)" )
                      '(display-time-interval 20))
;; require xalpha
(if (not (null window-system))
    (progn (require 'xalpha)
           (xalpha-init 0)
           (global-set-key [C-S-f5] (lambda nil (interactive) (xalpha-change -1)))
           (global-set-key [C-f5]   (lambda nil (interactive) (xalpha-change 1)))))

;; set full screen mode for darwin
(if (not (null window-system))
    (progn (if (eq system-type 'darwin)
               (progn (set-frame-position (selected-frame) 0 0)
                      (set-frame-size (selected-frame) 120 38)
                      (defun toggle-fullscreen()
                        (interactive)
                        (ns-toggle-fullscreen))
                      (global-set-key [f11] 'toggle-fullscreen)))))

;; enable transient-mark
(transient-mark-mode t)

;; enable auto-image-file
(auto-image-file-mode t)

;; set scroll-step
(setq scroll-margin 0 scroll-conservatively 10000)
(setq scroll-step 1)

;; close and kill current buffer immediately
(define-key global-map (kbd "C-x k") 'kill-this-buffer)

;; set default split threshold
(setq split-height-threshold 30)
(setq split-width-threshold 80)

(defun smart-split-current-window nil
  (let ((current (selected-window))
        (height (/ split-height-threshold 2))
        (target-window nil))
    (other-window 1)
    (setq selected (select-window (get-largest-window)))
    (if (and (>= (window-width selected) (* height 8))
             (>= (window-width selected) (* (window-height selected) 4)))
        (setq target-window (split-window-horizontally)))
    (if (not target-window)
        (setq target-window (split-window-vertically (- height))))
    (select-window current)
    target-window))

;; other window or split
(defun other-window-or-split nil
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-x o") 'other-window-or-split)

;; enlarge or shrink window
(global-set-key (kbd "C-+") 'enlarge-window)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-_") 'shrink-window)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)

;; select buffer
(require 'windmove)

(defun select-window-arrow(dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (if (not (or (eq other-window nil)
                 (eq other-window (minibuffer-window))))
        (select-window other-window))))

(define-key ctl-w-map [up]    '(lambda nil (interactive) (select-window-arrow 'up)))
(define-key ctl-w-map [down]  '(lambda nil (interactive) (select-window-arrow 'down)))
(define-key ctl-w-map [left]  '(lambda nil (interactive) (select-window-arrow 'left)))
(define-key ctl-w-map [right] '(lambda nil (interactive) (select-window-arrow 'right)))
(define-key ctl-w-map "o" 'other-window-or-split)


(define-key ctl-w-map (kbd "C-w") 'other-window-or-split)

;; require buffer-move
(require 'buffer-move)

(define-key ctl-w-map [(control up)]    'buf-move-up)
(define-key ctl-w-map [(control down)]  'buf-move-down)
(define-key ctl-w-map [(control left)]  'buf-move-left)
(define-key ctl-w-map [(control right)] 'buf-move-right)

;; settings for ibuffer
(require 'ibuffer-settings)

;; settings for ido-mdoe
;;(require 'ido-settings)

;; settings for split rotate buffers
(require 'split-rotate)

;; settings for switch buffers
(require 'switch-buffers)

;; settings for compilation
(require 'compilation-settings)

;; settings for completions
(require 'completions-settings)

;; settings for tabbar mode
(require 'tabbar-settings)

(provide 'view-config)
