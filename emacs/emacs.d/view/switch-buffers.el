;; -*- emacs-lisp -*-

;; -------------------- switch-buffers --------------------

(defun switch-buffers nil
  "Switch the two buffers in current window and next window."
  (interactive)
  (if (one-window-p)
      (message "There is only one window!")
    (let ((buffer1 (current-buffer))
          (buffer2 (window-buffer (next-window))))
      (switch-to-buffer buffer2)
      (set-window-buffer (next-window) buffer1))))

(define-key ctl-x-4-map "s" 'switch-buffers)

(provide 'switch-buffers)
