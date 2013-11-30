;; -*- emacs-lisp -*-

;; -------------------- tabbar settings --------------------

(require 'tabbar)
(require 'tabbar-extension)

;; enable tabbar mode
(tabbar-mode t)

;; toggle local mode
(global-set-key [C-f9] 'tabbar-local-mode)

;; set tabbar groups
(defun tabbar-buffer-groups nil
  "Return the list of group names BUFFER belongs to.
 Return only one group for each buffer."
  (list (cond
         ((string-match "^\\*.+\\*$" (buffer-name))
          "Emacs")
         ((eq major-mode 'dired-mode)
          "Dired")
         ((memq major-mode '(makefile-mode makefile-gmake-mode makefile-bsdmake-mode))
          "Makefile")
         (t
          "Code"))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; bind tabbar-forward/backward to Ctrl-page up/page dow
;; tabbar.el will override ctl-c-map, so we comment tabbar.el directly.
(global-set-key [M-S-down] 'tabbar-forward-group)
(global-set-key [M-S-up]   'tabbar-backward-group)
(global-set-key [M-down]   'tabbar-forward-tab)
(global-set-key [M-up]     'tabbar-backward-tab)
(global-set-key [C-next]   'tabbar-forward-tab)
(global-set-key [C-prior]  'tabbar-backward-tab)

;; set tabbar theme
(set-face-attribute 'tabbar-default nil
					:foreground "gray30"
					:background "gray80"
					:height 1.0
                    :box nil)

(set-face-attribute 'tabbar-button nil
					:inherit 'tabbar-default
                    :box nil)

(set-face-attribute 'tabbar-selected nil
					:inherit 'tabbar-default
					:foreground "maroon"
					:background "gray90"
					:underline "blue"
                    :box nil)

(set-face-attribute 'tabbar-unselected nil
					:inherit 'tabbar-default
                    :box nil)

(provide 'tabbar-settings)
