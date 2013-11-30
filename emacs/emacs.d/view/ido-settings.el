;; -*- emacs-lisp -*-

;; -------------------- ido-settings --------------------

;; enable ido mode
(ido-mode 1)

(setq ido-everywhere t)
(setq ido-define-mode-map-hook 'ido-setup-hook)

(global-set-key (kbd "C-x C-f") 'ido-find-file)
(setq ido-max-directory-size 1000000)

;; create a new buffer if no buffer matchs substring
(setq ido-create-new-buffer 'always)

;; customize the display order
(setq ido-file-extensions-order '(".c" ".cpp" ".h" ".hpp" ".py" ".org" ".el" ".sh"))

;; disable ido-completing-read interface for dired mode buffers
(add-hook 'dired-mode-hook
          '(lambda () (setq ido-enable-replace-completing-read nil)))

;; setup ido mode for ibuffer
(require 'ibuffer)

(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map "\C-x\C-f"
              'ibuffer-ido-find-file)))

(custom-set-faces '(ido-first-match
                    ((((class color)) (:bold nil :underline t :foreground "blue")))))

;; C-j: to open a directory or a buffer, just use C-j to prevent opening a new file
;; C-a: toggle display of the hidden buffers or files, use C-a
;; C-t: toggle a regexp match
;; C-b: reverts to the old switch-buffer completion engine
;; C-f: reverts to the old find-file completion engine
;; C-d: opens a dired buffer in the current directory
;; C-c: toggles if searching of buffer and file names should ignore case
;; TAB: attempt to complete the input like the normal completing read functionality
;; C-p: toggles prefix matching; when it’s on the input will only match the beginning of a filename instead of any part of it
;; C-s / C-r: moves to the next and previous match, respectively
;; C-t: toggles matching by Emacs regular expression
;; Backspace: deletes characters as usual or goes up one directory if it makes sense to do so.	All (but functionality varies)
;; C-SPC / C-@: restricts the completion list to anything that matches your current input. (Thanks to Joakim Hårsman for pointing it out)	All
;; //: like most *nix shells two forward slashes in a path means “ignore the preceding path, and go back to the top-most directory”. Works the same in Ido but it’s more interactive: it will go to the root / (or the root of the current drive in Windows)	Files
;; ~/: jumps to the home directory. On Windows this would be typically be %USERPROFILE% or %HOME%, if it is defined.
;; M-d: Searches for the input in all sub-directories to the directory you’re in.	Files
;; C-k: Kills the currently focused buffer or deletes the file depending on the mode.	Files / Buffers
;; M-m: Creates a new sub-directory to the directory you’re in

(provide 'ido-settings)
