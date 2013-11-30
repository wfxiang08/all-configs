;; -*- emacs-lisp -*-

;; -------------------- ibuffer settings --------------------

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("CODE"   (or (name . ".*\\.c$")
                             (name . ".*\\.h$")
                             (name . ".*\\.cpp$")
                             (mode . c-mode)
                             (mode . cc-mode)
                             (mode . c++-mode)
                             (mode . makefile-mode)
                             (mode . makefile-gmake-mode)
                             (mode . makefile-bsdmake-mode)
                             (mode . sh-mode)
                             (mode . python-mode)
                             (mode . asm-mode)))
               ("DIRED"      (mode . dired-mode))
               ("ORG"        (mode . org-mode))
               ("GNUS"   (or (mode . message-mode)
                             (mode . mail-mode)))
               ("ELISP"  (or (filename . "^\\.emacs$")
                             (filename . ".*\\.el$")))
               ("EMACS"   (or (name . "\*Help\*")
                             (name . "\*Apropos\*")
                             (name . "\*info\*")
                             (name . "\*scratch\*")
                             (name . "\*Messages\*")))))))

(add-hook 'ibuffer-mode-hook (lambda nil
                               (ibuffer-auto-mode 1)
                               (ibuffer-switch-to-saved-filter-groups "default")))

;; set human readable size column
(define-ibuffer-column size-h
  (:name "Size")
  (cond
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))))
                                        ;   (t (format "%8d" (buffer-size)))))

(setq ibuffer-formats (quote ((mark modified read-only " "
                                    (name 25 25 :left :elide) " "
                                    ;; (size-h 10 -1 :right) " "
                                    (size 10 0 :right) " "
                                    (mode 16 16 :left :elide) " "
                                    filename-and-process))))

;; sorted by name/pathname
(eval-after-load "ibuf-ext"
  '(define-ibuffer-filter filename
       "Toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
                   :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                        (buffer-local-value 'dired-directory buf))
       (string-match qualifier it))))

;; bind to C-x C-b, and set revert-buffer
(setq ibuffer-show-empty-filter-groups nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill a buffer without prompted
(setq ibuffer-expert t)

(provide 'ibuffer-settings)
