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

