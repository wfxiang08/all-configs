;; -*- emacs-lisp -*-

;; -------------------- latex-mode-settings --------------------

;; define latex-minor mode map
(defvar my-latex-minor-mode-map (make-keymap) "my-latex-minor-mode keymap.")

;; enable smart comment/uncomment shortcut for latex mode
(define-key my-latex-minor-mode-map (kbd "C-c /") 'smart-comment-uncomment)

(define-minor-mode my-latex-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " latex-c-map" 'my-latex-minor-mode-`map)

(add-hook 'latex-mode-hook
          (lambda ()
            (my-latex-minor-mode 1)))

(provide 'latex-mode-settings)
