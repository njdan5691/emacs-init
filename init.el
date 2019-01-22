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

(use-package dired-x
  :bind (("C-l C-o" . dired-omit-mode))
  :bind (("C-c D" . find-name-dired))
  :bind (("C-c o" . zz:dired-open-file))
  :commands (dired-dwim-target-directory)
  :config
  (defun zz:dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (progn
    (setq dired-omit-verbose nil)
    (setq-default dired-omit-files-p t) ; Buffer-local variable                                     
    ;;(setq dired-omit-files (concat dired-omit-files "\\|\\.ms$\\|\\.o$\\|^\\..+$"))               
    (setq dired-omit-files "^\\.[^.]\\|\\.ms$\\|\\.o$")
    ;; hide backup, autosave, *.*~ files                                                            
    ;; omit mode can be toggled using `M-o' in dired buffer                                         
    (add-hook 'dired-mode-hook #'dired-omit-mode)))

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

