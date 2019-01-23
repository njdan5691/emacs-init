;;; Example File


(setq-default
 tab-width 2
 indent-tabs-mode t
 user-emacs-directory (expand-file-name "~/.emacs.d")
 c-default-style "linux"
 sh-indentation 2
 sh-basic-offset 2
 julia-indent-offset 2
 js-indent-level 2
 c-basic-offset 2)

(setq
 dired-listing-switches "-alGhvF --group-directories-first"
 nmake-command "nmake"
 inhibit-splash-screen t
 initial-scratch-message nil
 enable-local-variables :all
 counsel-grep-base-command "ag --nocolor %s %s"
 compilation-last-buffer nil
 compilation-skip-threshold 2
 save-interprogram-paste-before-kill t
 blink-cursor-mode -1
 ring-bell-function 'ignore
 history-length 2000
 history-delete-duplicates t
 enable-recursive-minibuffers t                                                                      
 backward-delete-char-untabify-method 'hungry
 electric-indent-mode nil
 create-lockfiles nil
 apropos-do-all t)

(require 'package)                                                                                  
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize 'noactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(package-initialize)
(defun package--save-selected-packages (&rest opt) nil)

(require 'use-package)
(require 'bind-key)
(require 'dired-x)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)



(bind-keys :map global-map
           :prefix-map zz:idev-prefix
           :prefix "C-l")

(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f10>"))

(bind-keys :map global-map
           ("C-z" . save-buffers-kill-emacs)
           ("C-x k" . kill-this-buffer)
           ("<f1>" . menu-bar-open)
           ("M-1" . menu-bar-open)
           ("<f2>" . shell-command)
           ("M-2" . shell-command)
           ("<f4>" . grep)
           ("M-4" . grep)
           ("<f7>" . copy-to-register)
           ("M-7" . copy-to-register)
           ("<f8>" . insert-register)
           ("M-8" . insert-register)                                                               
           ("<f10>" . package-list-packages)
           ("M-0" . package-list-packages)
           ("C-+" . text-scale-increase)
           ("C--" . text-scale-decrease)
           ("C-c r" . replace-string)
           ("C-c R" . replace-regexp)
           ("C-c t" . zz:toggle-tab-width)
           ("C-c q" . query-replace)
           ("C-c Q" . query-replace-regexp)
           ("C-c a" . apropos-command)
           ("C-x C-o" . find-file))

(use-package el-get
  :defer t
  :ensure t)

(el-get-bundle njdan5691/elisp-misc)                                                               

(use-package engine-mode
  ;; default keymap is bound to "C-x /"
  :config
  (require 'format-spec)
  (progn
    (setq engine/browser-function 'eww-browser-url)
    (defengine google
      "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g"
      :browser 'eww-browse-url
      )
    (defengine gist-github
      "https://gist.github.com/search?utf=1&&q=%s"
      :keybinding "G"
      :browser 'eww-browse-url                                                                     
      )
    (defengine dicionary
      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s\
"
      :keybinding "d"
      :browser 'eww-browse-url
      )
    (engine-mode 1))
  :ensure t)


(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))




(use-package compile
  :config
  (setq nmake-command "nmake"))

;; Save minibuffer histories
(use-package savehist
  :config
  (savehist-mode))

;; displays the key bindings following your currently entered incomplete command
(use-package which-key
  :init
  (setq which-key-side-window-max-height 0.40)
  (which-key-mode)
  :ensure t)

(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "places" user-emacs-directory))
  (save-place-mode))

(use-package recentf
  :config
  (progn
    (setq recentf-exclude '("^/var/folders\.*"
                            "COMMIT_EDITMSG\'"
                            ".*-autoloads\.el\'"
                            "[/\]\elpa/"))
    (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
          recentf-max-saved-items 15)))

(use-package grep                                                                                                                     
  :config
  (setq grep-use-null-device nil)
  (grep-apply-setting 'grep-command '("ag --depth 0 --vimgrep " . 24)))

(use-package cperl-mode
  :config (progn
            (setq cperl-indent-level 2)
            (defalias 'perl-mode 'cperl-mode))
  :init
  :mode "\\.\\(cgi\\|psgi\\|t\\|pl\\)$")

(use-package yankpad
  :config
  (setq yankpad-file (expand-file-name "~/.emacs.d/yankpad.org"))
  :ensure t)


(use-package cc-mode
  :config
  (defun my-c-mode-hook ()
    (global-hl-line-mode -1)
    (setq-local eldoc-echo-area-use-multiline-p t)
    (bind-keys :map global-map
               :prefix-map zz:ggtags-prefix
               :prefix "C-c g")
    (bind-keys :map global-map
               :prefix-map zz:yankpad-prefix
               :prefix "C-c y")
    (defun zz:select-snippet ()
      (interactive)
      (unless yankpad-category
        (or (yankpad-local-category-to-major-mode)
            (yankpad-set-category)))
      (let ((name (ivy-read "Snippet:" (yankpad-active-snippets))))
        (let ((snippet (assoc name (yankpad-active-snippets))))
          (if snippet
              (yankpad--run-snippet snippet)
            (message (concat "No snippet named " name))))))

    (bind-keys :map c-mode-base-map
               ("C-l TAB" . elispm:reformat-buffer)
               ("C-l z" . zz:indent-with-gnu-indent)
               ("C-l x" . c-mark-function)
               ("C-l c" . compile)
               ("C-c t" . elispm:toggle-tab-width)
               ("C-c g d" . ggtags-find-definition)                                                                                   
               ("C-c g o" . ggtags-find-other-symbol)
               ("C-c g e" . ggtags-find-tag-regexp)
               ("C-c g g" . ggtags-grep)
               ("C-c g r" . ggtags-find-reference)
               ("C-c g f" . counsel-gtags-find-file)
               ("C-c y a" . yankpad-append-category)
               ("C-c y e" . yankpad-edit)
               ("C-c y c" . yankpad-capture-snippet)
               ("C-c y x" . zz:select-snippet)
               ("C-c y r" . yankpad-reload)
               ("C-c y i" . yankpad-insert)
               ("C-c y s" . yankpad-set-category)
               ("M-." . ggtags-find-tag-dwim)
               ("M-?" . ggtags-show-definition)
               ("M-," . ggtags-prev-mark)
               ("M-p" . beginning-of-defun)
               ("M-n" . end-of-defun))

    (setq comment-start "// "
          comment-end ""
          show-trailing-whitespace t)
    (c-toggle-auto-state 1)
    (c-toggle-hungry-state 1)
    (setq ac-sources (delete 'ac-source-dictionary ac-sources))
    (c-set-offset 'comment-intro 0)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-cont-nonempty '+)
    (transient-mark-mode 1))
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
  (add-hook 'c-mode-common-hook #'elispm:my-auto-complete-disabling-hook)
  (add-hook 'c-mode-common-hook #'my-c-mode-hook))


(use-package easy-kill
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-paragraph] 'easy-mark)
  :defer t
  :ensure t)

(use-package smex
  :defer t
  :ensure t
  :after ivy
  :commands smex-initialize
  :config
  (smex-initialize))

(use-package ctable
  :defer t
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :defer t
  :commands aggressive-indent-mode
  :diminish aggressive-indent-mode
  :config
  (aggressive-indent-mode t))     

(use-package swiper
  :defer t
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; This lets you start out in isearch and drop into swiper
  ;; :bind (:map isearch-mode-map
  ;;            ("M-i" . swiper-from-isearch)) ; isearch > swiper

  :bind (
         ;([remap isearch-forward] . swiper)
         ;([remap isearch-backward] . swiper)
         ("C-s" . swiper)
         ("C-r" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("M-i" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ))



(use-package ggtags
  :diminish ggtags-mode
  :config
  (progn
    (setq ggtags-use-sqlite3 1)
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1)))))
  :ensure t)

(use-package counsel-gtags
  :defer t
  :ensure t
  :requires swiper)


(use-package ivy
  :defer t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x j" . ivy-kill-line)
         ("C-h v" . counsel-describe-variable)
         ("C-h s" . counsel-set-variable)
         ("C-h f" . counsel-describe-function))
  :config
  (setq ivy-use-virtual-buffers t)
  :ensure t)                                                                                        

(use-package lisp-mode
  :defer t
  :config
  (defun zz:disable-tabs () (setq indent-tabs-mode nil))
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t)
    (add-hook 'list-mode-hook 'zz:disable-tabs)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'zz:disable-tabs)
    (bind-key "M-." 'find-function-at-point)))

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
  :requires swiper
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

