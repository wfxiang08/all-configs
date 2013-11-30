;; -*- emacs-lisp -*-

;; -------------------- xalpha --------------------

(setq xalpha-stack '(100 90 80 50) xalpha-pop nil)
(setq xalpha (pop xalpha-stack))

(defun xalpha-up nil
  (cond ((not (null xalpha-pop))
         (push xalpha xalpha-stack)
         (setq xalpha (pop xalpha-pop)))))

(defun xalpha-down nil	
  (cond ((not (null xalpha-stack))
         (push xalpha xalpha-pop)
         (setq xalpha (pop xalpha-stack)))))

(defun transform-window nil
  (set-frame-parameter (selected-frame) 'alpha xalpha)
  (message (format "X-Window xalpha (%d)." xalpha)))

(defun xalpha-change(arg)
  (if (eq arg 0)
	 	(transform-window)
      (cond ((not (null (if (> arg 0) (xalpha-up) (xalpha-down))))
			   (transform-window)))))

(defun xalpha-init(arg)
  (let ((x 0))
    (while (< x arg)
      (xalpha-down)
      (setq x (+ x 1))))
  (xalpha-change 0))

(provide 'xalpha)
