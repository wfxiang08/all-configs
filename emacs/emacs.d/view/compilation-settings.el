;; -*- emacs-lisp -*-

;; -------------------- compilation settings --------------------  

;; auto-scroll while compiling
(setq compilation-scroll-output t)

;; special display function for compilation
(add-to-list 'special-display-buffer-names '("*compilation*" smart-display-compilation))

;; split and resize window
(defun smart-display-compilation(buf &optional args)
  (setq target-window nil)
  (setq sel (get-buffer "*compilation*"))
  (if (not (eq sel nil))
      (setq target-window (get-buffer-window sel)))
  (if (not target-window)
      (setq target-window (smart-split-current-window)))
  (set-window-buffer target-window buf)
  target-window)

;; kill compilation buffer
(defun compilation-kill-buffer (buf)
  (interactive)
  (delete-windows-on buf))

;; terminate compilation buffer if compile succeed (after 2 seconds)
(defun kill-buffer-when-compile-success (process)
  "Close current buffer when `shell-command' exit."
  (set-process-sentinel process (lambda (proc change)
                                  (when (string-match "finished" change)
                                    (progn
                                      (let ((buf (process-buffer proc)))
                                        (run-at-time 2 nil 'compilation-kill-buffer buf)))))))

(add-hook 'compilation-start-hook 'kill-buffer-when-compile-success)

(defun smart-compile nil
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (ignore-errors (kill-compilation))
    (if (not (ignore-errors (recompile)))
        (call-interactively 'compile))))

(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key [f5] 'smart-compile)

(provide 'compilation-settings)
