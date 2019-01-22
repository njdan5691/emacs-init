;;; Example File

(use-package eshell
  :config
  (setq eshell-prompt-function                                                                      
        (lambda nil " $ "))
  (setenv "PAGER" "cat"))

(use-package csv-mode                                                                               
  :mode "\\.csv\\'"
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure)

(use-package kmacro
  :config
  (defun zz:macro-query (arg)
    "Prompt for input using minibuffer during kbd macro execution.                                 \
                                                                                                    
With prefix argument, allows you to select what prompt string to use.                              \
                                                                                                    
If the input is non-empty, it is inserted at point."
    (interactive "P")
    (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
           (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                    (read-from-minibuffer prompt))))
      (unless (string= "" input) (insert input))))

  (global-set-key "\C-xQ" 'zz:macro-query))


(use-package yankpad
  :config
  (setq yankpad-file (expand-file-name "~/.emacs.d/yankpad.org"))
  :ensure t)

(use-package files
  :bind (("C-l C-k" . elispm:kill-other-buffers))
  :config
  (progn
    (setq require-final-newline t
          confirm-kill-emacs nil
          confirm-nonexistent-file-or-buffer nil
          backup-directory-alist `(("." . "~/.backups.emacs.d"))
          delete-old-versions t
          kept-new-versions 10
          kept-old-versions 0
          version-control t)))

