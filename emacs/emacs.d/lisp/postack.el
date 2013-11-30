;;; Maintain a stack of buffer positions, useful when recusively
;;; looking up function definitions.

(defvar postack-stack '() "The position stack")

(defgroup postack nil
  "Interface for postack"
  :prefix "postack-"
  :group 'tools)

(defcustom postack-record-marker nil
  "a temporary marker, call postack-record-push won't be pushed into postack unless it's non-nil"
  :type 'marker
  :group 'postack)

(defun postack-record-set ()
  "recort current point-marker"
  (interactive)
  (setq postack-record-marker (point-marker)))

(defun postack-record-push ()
  (interactive)
  (if postack-record-marker
      (let ((pos postack-record-marker))
        (setq postack-record-marker nil)
        (setq postack-stack (cons pos postack-stack))
        (message (format "Marked: (%s:%s)" (marker-buffer pos) (marker-position pos))))))

(defun postack-goto (marker)
  "Should be marker-goto."
  (switch-to-buffer (marker-buffer pos))
  (goto-char (marker-position pos)))

(defun postack-push ()
  "Push the current position on the position stack."
  (interactive)
  (let ((pos (point-marker)))
    (setq postack-stack (cons pos postack-stack))
    (message (format "Marked: (%s:%s)" (marker-buffer pos) (marker-position pos))) ))

(defun postack-pop ()
  "Remove the top position from the position stack and make it current."
  (interactive)
  (let ((pos (car postack-stack)))
    (setq postack-stack (cdr postack-stack))
    (cond ((null pos)
           (message "Position stack empty"))
          ((markerp pos)
           (postack-goto pos)
           (message (format "Position: (%s:%s)" (marker-buffer pos) (marker-position pos))))
          (t
           (message "Invalid position in stack")) ) ))

(provide 'postack)

