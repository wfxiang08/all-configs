;; -*- emacs-lisp -*-

;; -------------------- title-time --------------------

(require 'time)

(defun get-frame-title nil
  (let ((base-name "%s"))
    (if (buffer-file-name)
        (format base-name "%f")
      (if (eql major-mode 'dired-mode)
          (format base-name (second (split-string (pwd))))
        (format base-name "%b")))))

(defvar title-time-mode t
   "This is set to t iff we are displaying the current time in the title bar.")

(defun title-time-set nil
  "Set `frame-title-format' to the local system name followed by date,
   time, and load information (as per `display-time-string-forms') and perhaps
   followed by an appointment notification."
  (let ((title-name (get-frame-title)))
    (setq frame-title-format
          (concat  "Emacs    " title-name
                   "         " display-time-string))))

(defun title-time-update nil
  "Update the time display in the title-bar.
   Skips inferior frames, that is, those without a minibuffer (eg. speedbar). "
  (interactive)
  
  ;; remove time display from the mode line
  (delq 'display-time-string global-mode-string)
  (delq 'appt-mode-string global-mode-string)
  
  (let ((start-frame (selected-frame)))
    (save-excursion
      (save-window-excursion
        (let ((my-frame-list (frame-list))
              (my-frame nil))
          (while (setq my-frame (car my-frame-list))
            (when (frame-parameter my-frame 'minibuffer)
              (make-variable-frame-local 'frame-title-format)
              (select-frame my-frame)
              (title-time-set))
            (setq my-frame-list (cdr my-frame-list))))))
    (select-frame start-frame)))

(add-hook 'display-time-hook #'title-time-update)

(provide 'title-time)
