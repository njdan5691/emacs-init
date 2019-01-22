;;; Example File

(use-package recentf-ext
  :ensure t)

(use-package flyspell
  :config
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package ibuffer-mode
  :bind ("C-x C-b" . ibuffer))

(use-package shell
  :config
  (defun zz:comint-init()                                                                           
    (setq comint-process-echoes t))
  (add-hook 'comint-mode-hook 'zz:comint-init))

(use-package makefile
  :mode (("\\.nmk\\'" . makefile-gmake-mode)))

(use-package  counsel
  :defer t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf))
  :ensure t)

;; (use-package org-mode
;;  :mode "\\.org$"
;;  :ensure org
;;  :init
;;  (progn
;;    (setq org-startup-truncated nil)))

(bind-keys :map global-map
           :prefix-map my-prefix-map-l
           :prefix "C-l"
           ("s" . isearch-forward)
           ("r" . isearch-backward)
           ("l" . load-file)
           ("C-l" . recenter))

(use-package vimish-fold
  :bind (("C-c H" . vimish-fold)
         ("C-c h" . vimish-fold-delete))
  :ensure t)

(use-package diminish                                                                                                  
  :defer t
  :ensure t)

(use-package auto-complete
  :diminish
  :config
  (progn
    (setq ac-modes '(cc-mode c-mode c++-mode))
    (ac-config-default)
    (setq ac-delay 0.8))
  :ensure t)

(use-package rainbow-delimiters
  :defer t
  :ensure t)

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

