(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(if (eq window-system 'x)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (global-set-key (kbd "C-+") 'text-scale-increase)
      (global-set-key (kbd "C--") 'text-scale-decrease)
      (set-background-color "black")
      (set-foreground-color "white")
      ;;(set-face-attribute 'default t :font "Consolas-20")
      ;;(set-face-attribute 'default t :font "Office Code Pro-26")
      (set-default-font "Consolas-20")))

;; If the variable is a buffer-local, and you want it the
;; same in all types of buffers add it here.
(setq-default
 tab-width 2
 indent-tabs-mode t)

(setq
 inhibit-splash-screen t
 initial-scratch-message nil
 package--init-file-ensured t
 enable-local-variables :all
 user-emacs-directory (expand-file-name "~/.emacs.d")
 save-interprogram-paste-before-kill t
 blink-cursor-mode -1
 ring-bell-function 'ignore                                                                     
 backward-delete-char-untabify-method 'hungry
 create-lockfiles nil
 apropos-do-all t)

(require 'package)                                                                                  
(setq package-enable-at-startup nil)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)
(defun package--save-selected-packages (&rest opt) nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish                                                                                                  
  :defer t
  :ensure t)

(use-package quelpa-use-package
  :defer t
  :ensure t
  :init
    (setq quelpa-checkout-melpa-p nil)
    (setq quelpa-update-melpa-p nil))

(require 'quelpa-use-package)

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

;; Since I almost never use C-l to recenter my emacs session,
;; I repurpose C-l as prefix, so I can bind some of my more frequently
;; used function. And Bind recenter to C-l C-l.
(bind-keys :map global-map
           :prefix-map zz:my-prefix
           :prefix "C-l"
           ("C-l" . recenter))

(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "C-z"))

(use-package elisp-misc
  :bind (("C-c TAB" . elispm:reformat-buffer)
         ("C-c C-k" . elispm:kill-other-buffers)
         ("C-c t" . elispm:toggle-tab-width))
  :quelpa (elisp-misc :fetcher github :repo "njdan5691/elisp-misc"))

(use-package engine-mode
  ;; default keymap is bound to "C-x /"
  :defer t
  :config
  (require 'format-spec)
  (progn
    (setq engine/browser-function 'eww-browser-url)
    (defengine google
      "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g"
      :browser 'eww-browse-url)
    (defengine gist-github
      "https://gist.github.com/search?utf=1&&q=%s"
      :keybinding "G"
      :browser 'eww-browse-url)
    (defengine dicionary
      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
      :keybinding "d"
      :browser 'eww-browse-url)
    (engine-mode 1))
  :ensure t)

;; displays the key bindings following your currently entered incomplete command
(use-package which-key
  :init
  (setq which-key-side-window-max-height 0.40)
  (which-key-mode)
  :ensure t)

;; Package Graveyard, packages I no longer use.

;; (use-package breadcrumb
;;   :ensure t
;;   :quelpa (breadcrumb :fetcher github :repo "pheaver/breadcrumb"))

;; (use-package org-manage
;;   :ensure t
;;   :quelpa (org-manage :fetcher github :repo "dmgerman/org-manage"))

;;(use-package el-get
;;  :defer t
;;  :ensure t)

;; Never use this, comment it out for future removal
;;(use-package ibuffer-mode
;;  :bind ("C-x C-b" . ibuffer))

;; (use-package cperl-mode
;;   :config (progn
;;             (setq cperl-indent-level 2)
;;             (defalias 'perl-mode 'cperl-mode))
;;   :init
;;   :mode "\\.\\(cgi\\|psgi\\|t\\|pl\\)$")

;; select text to be hightlighted on the screen
;; (use-package hi-lock
;;   :bind (("M-o l" . highlight-lines-matching-regexp)
;;          ("M-o r" . highlight-regexp)
;;         ("M-o w" . highlight-phrase)))

;;(use-package ctable
;;  :defer t
;;  :ensure t)

;; (use-package vimish-fold
;;   :bind (("C-c H" . vimish-fold)
;;          ("C-c h" . vimish-fold-delete))
;;   :ensure t)

;;(use-package eshell
;;  :config
;;  (setq eshell-prompt-function                                                                      
;;        (lambda nil " $ "))
;;  (setenv "PAGER" "cat"))

;; (use-package csv-mode                                                                               
;;   :mode "\\.csv\\'"
;;   :ensure t)

;; (use-package org-mode
;;  :mode "\\.org$"
;;  :ensure org
;;  :init
;;  (progn
;;    (setq org-startup-truncated nil)))

(use-package yankpad
  :ensure t
  :bind
  (
   :map global-map
   :prefix-map zz:yankpad-prefix
   :prefix "C-c y"
   ("a" . yankpad-append-category)
   ("e" . yankpad-edit)
   ("m" . yankpad-map)
   ("c" . yankpad-capture-snippet)
   ("x" . zz:select-snippet)
   ("r" . yankpad-reload)
   ("i" . yankpad-insert)
   ("s" . yankpad-set-category))
  :init
  (setq yankpad-file (expand-file-name "~/.emacs.d/yankpad.org"))
  :config
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
  :ensure ivy)

(use-package easy-kill
  :defer t
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-paragraph] 'easy-mark)
  :ensure t)

(use-package smex
  :defer t
  :ensure t
  :after ivy
  :commands smex-initialize
  :config
  (smex-initialize))

(use-package aggressive-indent
  :ensure t
  :defer t
  :hook (prog-mode . aggressive-indent-mode)
  :diminish aggressive-indent-mode)   

(use-package swiper
  :defer t
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq  counsel-grep-base-command "ag --nocolor %s %s")
  (setq ivy-count-format "(%d/%d) ")

  ;; This lets you start out in isearch and drop into swiper
  ;; :bind (:map isearch-mode-map
  ;;            ("M-i" . swiper-from-isearch)) ; isearch > swiper

  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("M-i" . counsel-imenu)
         ("M-x" . counsel-M-x)))

(use-package ggtags
  :defer t
  :diminish ggtags-mode
  :bind (("C-c g d" . ggtags-find-definition)
         ("C-c g o" . ggtags-find-other-symbol)
         ("C-c g e" . ggtags-find-tag-regexp)
         ("C-c g g" . ggtags-grep)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g f" . counsel-gtags-find-file))
  :config
  (progn
    (bind-keys :map global-map
               :prefix-map zz:ggtags-prefix
               :prefix "C-c g")

    (setq ggtags-use-sqlite3 1)                                                                     
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
    (add-hook 'dired-mode
              (lambda ()
                gg-tags-mode 1))
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

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package counsel
  :defer t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf))
  :requires swiper
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
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
   (setq show-paren-mode 1
        electric-pair-mode 1))

(use-package paredit
  :defer t
  :diminish
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))

(use-package recentf-ext
  :defer t
  :ensure t)

;; Configuration for packages that are part of emacs installation

(use-package cc-mode
  :defer t
  :ensure auto-complete
  :config
  (defun my-c-mode-hook ()
    (global-hl-line-mode -1)
    (abbrev-mode -1)
    (setq-local eldoc-echo-area-use-multiline-p t)
    (setq c-basic-offset 2
          c-default-style "linux")
    (bind-keys :map global-map
               :prefix-map zz:ggtags-prefix
               :prefix "C-c g")
    (bind-keys :map c-mode-base-map
               ("C-c z" . zz:indent-with-gnu-indent)
               ("C-c x" . c-mark-function)
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
  (add-hook 'c-mode-common-hook #'elispm:my-auto-complete-disabling-hook)
  (add-hook 'c-mode-common-hook #'my-c-mode-hook))

(use-package kmacro
  :defer t
  :ensure nil
  :config
  (defun zz:macro-query (arg)
    "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
    (interactive "P")
    (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
           (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                    (read-from-minibuffer prompt))))
      (unless (string= "" input) (insert input))))

  (global-set-key "\C-xQ" 'zz:macro-query))

(use-package hl-line
  :ensure nil
  :config
  (set-face-background 'hl-line "darkgreen")
  :hook (after-init . global-hl-line-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package lisp-mode
  :defer t
  :config
  (defun zz:disable-tabs () (setq indent-tabs-mode nil))
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t)
    (add-hook 'list-mode-hook 'zz:disable-tabs)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'zz:disable-tabs)
    (bind-key "M-." 'find-function-at-point)))

(use-package flyspell
  :defer t
  :hook (text-mode . flyspell-mode))

(use-package makefile
  :defer t
  :ensure nil
  :mode (("\\.nmk\\'" . makefile-gmake-mode)))

(use-package dired
  :defer t
  :ensure nil
  :commands (dired-dwim-target-directory)
  :bind (("C-l D" . find-name-dired)
         ("C-c o" . zz:dired-open-file)
         ("C-c C-o" . dired-omit-mode))
  :config
  (defun zz:dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (setq dired-listing-switches "-alGhvF --group-directories-first"))   

(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose nil)
    (setq dired-omit-files "^\\.[^.]\\|\\.ms$\\|\\.o$")                                                              
    (add-hook 'dired-mode-hook #'dired-omit-mode)))

(use-package minibuffer
  :ensure nil
  :config
  (setq history-length 2300                                                                         
        enable-recursive-minibuffers t
        history-delete-duplicates t))

(use-package grep                                                                                   
  :defer t
  :ensure nil
  :bind (("<f4>" . grep)
         ("M-4" . grep))
  :config
  (setq grep-use-null-device nil)
  (grep-apply-setting 'grep-command '("ag --depth 0 --vimgrep " . 24)))

(use-package simple
  :ensure nil
  :bind (("<f2>" . shell-command)
         ("M-2" . shell-command))
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Save minibuffer histories
(use-package savehist
  :config
  (savehist-mode))

(use-package replace
  :ensure nil
  :bind (("C-c r" . replace-string)
         ("C-c R" . replace-regexp)
         ("C-c q" . query-replace)
         ("C-c Q" . query-replace-regexp)))

(use-package menu-bar
  :ensure nil
  :bind (("C-x k" . kill-this-buffer)
         ("<f1>" . menu-bar-open)
         ("M-1" . menu-bar-open)))

(use-package register
  :ensure nil
  :bind (("<f7>" . copy-to-register)
         ("M-7" . copy-to-register)
         ("<f8>" . insert-register)
         ("M-8" . insert-register)))

(use-package isearch
  :ensure nil
  :bind (("C-l C-s" . isearch-forward)
         ("C-l C-r" . isearch-backward)))

(use-package saveplace
  :ensure nil
  :defer t
  :config
  (setq save-place-file (expand-file-name "places" user-emacs-directory)
        save-place-forget-unreadable-files nil
        save-place-limit 60)                                                                        
  :init
  (save-place-mode))

(use-package recentf
  :defer t
  :config
  (progn
    (setq recentf-exclude '("^/var/folders\.*"
                            "COMMIT_EDITMSG\'"
                            ".*-autoloads\.el\'"
                            "[/\]\elpa/"))
    (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
          recentf-max-saved-items 15)))

(use-package shell
  :defer t
  :bind (("C-l $" . shell))
  :ensure nil
  :config
  (defun zz:comint-init()
    (setq comint-process-echoes t))
  (add-hook 'comint-mode-hook 'zz:comint-init))

(use-package sh-script
  :ensure nil
  :defer t
  :config
  (setq sh-indentation 2
        sh-basic-offset 2))

(use-package compile
  :bind (("C-l c" . compile))
  :config
  (setq nmake-command "nmake"
        compilation-last-buffer nil
        compilation-skip-threshold 2))

(use-package apropos
  :ensure nil
  :bind (("C-c a" . apropos-command))) 

(use-package files
  :ensure nil
  :bind (("C-l l" . load-file)
         ("C-x C-o" . find-file))
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

;; This enables the downcase-region command, which is disabled
;; by default.
(put 'downcase-region 'disabled nil)

(defun display-startup-echo-area-message ()
  (message "Initialization Completed"))
