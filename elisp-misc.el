;;; elisp-misc.el --- Dan Misc elisp functions
;;
;; Author: Daniel <njdan.5691@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; No Comments

;;; Code:

;;;###autoload
(defun elispm:my-auto-complete-disabling-hook ()
  "Check to see if we should disable auto-complete-mode, or font-lock-mode."
  (save-excursion
    (when (re-search-forward "No auto-complete" 1000 t)
      (auto-complete-mode -1)))
  (save-excursion
    (when (re-search-forward "No font-lock" 1000 t)
      (font-lock-mode -1))))

;;;###autoload
(defun elispm:kill-other-buffers ()
   "Switch to scratch buffer then Kill all other buffers."
   (interactive)
   (switch-to-buffer "*scratch*")
   (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
   (cd (expand-file-name "~/")))


;;;###autoload
(defun elispm:reformat-buffer()
  "Use 'indent-region to indent the whole buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))

;;;###autoload
(defun elispm:toggle-tab-width ()                                                                     
  (interactive)
  (cond ((= 8 tab-width) (setq tab-width 1))
        ((= 1 tab-width) (setq tab-width 2))
        ((= 2 tab-width) (setq tab-width 4))
        ((= 4 tab-width) (setq tab-width 6))
        ((= 6 tab-width) (setq tab-width 8))))

;;;###autoload
(defun elispm:find-file-init ()    
  "Function to visit init.el"
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))


;;;###autoload
(defun elispm:find-file-hints ()
  "Function to visit Hints"
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/Hints.txt")))



;;;###autoload
(defun elispm:align-table (beg end)
  (interactive "r")
  (flush-lines "^$" beg end)
  (replace-string " |" "|" nil beg end)
  (replace-string "| " "|" nil beg end)
  (align-regexp beg end "\\(\\s-*\\)|" 1 0 "y")
  (sort-lines t beg end))

;;;###autoload
(defun elispm:unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string "'" "&apos;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))

;;;###autoload
(defun elispm:read-mb-lines (prompt some-keyseq)
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "RET") 'newline)
    (define-key keymap some-keyseq 'exit-minibuffer)
    (read-from-minibuffer prompt nil keymap)))

;;;###autoload
(defun elispm:simplified-read-mb-lines (prompt)
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "RET") 'newline)
    (read-from-minibuffer prompt nil keymap)))


;;;###autoload
(defun elispm:simplified-read-mb-lines-def (prompt def)
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "RET") 'newline)
    (read-from-minibuffer prompt def keymap)))

;;;###autoload
(defun elm:remove-lines (pattern &optional b e)
  (interactive "*sPattern:\nr")
  (save-excursion
      (flush-lines pattern b e)))



(provide 'elisp-misc)
;;; elisp-misc.el ends here
