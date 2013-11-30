;; -*- emacs-lisp -*-

;; -------------------- taglist --------------------

(require 'speedbar)

(defun quicksort (lst cmp)
  (if (null lst) nil
    (let* ((spl (car lst))
           (rst (cdr lst))
           (smalp (lambda (x)
                    (funcall cmp x spl))))
      (append (quicksort (remove-if-not smalp rst) cmp)
              (list spl)
              (quicksort (remove-if smalp rst) cmp)))))

(defun taglist-create-tags (sort-type)
  (let ((tags (speedbar-fetch-dynamic-etags (buffer-file-name taglist-source-buffer))))
    (if (> sort-type 0)
        (setq tags (quicksort tags '(lambda (tag0 tag1) (string-lessp (car tag0) (car tag1)))))
      (setq tags (quicksort tags '(lambda (tag0 tag1) (< (cdr tag0) (cdr tag1))))))
    tags))

(defun taglist-insert (tags source-buffer current-line)
  (toggle-read-only 0)
  (delete-region 1 (point-max))
  (let ((list-pos 0))
    (while tags
      (insert (format "    %s L" (buffer-name source-buffer)))
      (let ((tag-line
             (with-current-buffer source-buffer
               (line-number-at-pos (cdar tags)))))
        (insert (format "%-6s  " (format "%d:" tag-line)))
        (if (>= current-line tag-line)
            (setq list-pos
                  (1+ list-pos))))
      (insert (caar tags))
      (insert "\n")
      (setq tags (cdr tags)))
    (goto-line list-pos)
    (toggle-read-only 1)))

(defun taglist nil
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq taglist-source-buffer (current-buffer))
        (setq taglist-current-line (line-number-at-pos))
        (let ((tags (taglist-create-tags taglist-sort-type)))
          (ignore-errors (kill-buffer "*etags tmp*"))
          (setq taglist-buffer (get-buffer "*etags list*"))
          (if (not taglist-buffer)
              (setq taglist-buffer (get-buffer-create "*etags list*")))
          (set-buffer taglist-buffer)
          (taglist-insert tags taglist-source-buffer taglist-current-line)
          (setq taglist-window (get-buffer-window taglist-buffer))
          (if (not taglist-window)
              (setq taglist-window (smart-split-current-window)))
          (set-window-buffer taglist-window taglist-buffer)
          (select-window taglist-window)
          (taglist-mode)))))

(defvar taglist-mode-hook nil)

(defvar taglist-keywords
  (list (list "^[\t ]*\\([^ ]*\\) \\(L[0-9]+\\):[\t ]*\\(.*\\)$" 1 font-lock-keyword-face)
        (list "^[\t ]*\\([^ ]*\\) \\(L[0-9]+\\):[\t ]*\\(.*\\)$" 2 font-lock-comment-delimiter-face)
        (list "^[\t ]*\\([^ ]*\\) \\(L[0-9]+\\):[\t ]*\\(.*\\)$" 3 font-lock-function-name-face)))

(defvar taglist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") '(lambda nil (interactive) (taglist-jump nil)))
    (define-key map (kbd "RET")        '(lambda nil (interactive) (taglist-jump t)))
    (define-key map (kbd "q")          'taglist-quit)
    (define-key map (kbd "s")          'taglist-sort)
    map))

(defun taglist-kill nil
  (ignore-errors (kill-buffer "*etags tmp*"))
  (let ((buffer (get-buffer "*etags list*")))
    (while (setq window (get-buffer-window buffer))
      (delete-window window))
    (kill-buffer buffer)))

(defun taglist-jump (kill)
  (interactive)
  (let ((line (buffer-substring
               (line-beginning-position)
               (line-end-position)))
        (taglist-window (selected-window)))
    (string-match "^[\t ]*\\([^ ]*\\) L\\([0-9]+\\):[\t ]*.*$" line)
    (let ((buffer (get-buffer (match-string 1 line))))
      (if (not (eq buffer nil))
          (setq window (get-buffer-window buffer)))
      (if (not window)
          (setq window (smart-split-current-window)))
      (set-window-buffer window buffer)
      (select-window window)
      (goto-line (string-to-number (match-string 2 line))))
    (if kill
        (taglist-kill)
      (select-window taglist-window))))

(defun taglist-quit nil
  (interactive)
  (taglist-kill))

(defvar taglist-sort-type 0)
(defvar taglist-source-buffer nil)
(defvar taglist-current-line 0)

(defun taglist-sort nil
  (interactive)
  (setq taglist-sort-type (- 1 taglist-sort-type))
  (let ((tags (taglist-create-tags taglist-sort-type)))
    (taglist-insert tags taglist-source-buffer taglist-current-line)))

(defun taglist-mode nil
  (interactive)
  (kill-all-local-variables)
  (use-local-map taglist-map)
  (setq major-mode 'taglist-mode)
  (setq mode-name "Tag-List")
  (setq font-lock-defaults
        (list 'taglist-keywords))
  (run-mode-hooks 'taglist-mode-hook))

(global-set-key [f9] 'taglist)

(provide 'taglist)
