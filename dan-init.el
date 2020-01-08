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

(unless (eq window-system 'x)
  (xterm-mouse-mode))

;; If the variable is a buffer-local, and you want it the
;; same in all types of buffers add it here.
(setq-default
 tab-width 2
 indent-tabs-mode t)

(setq
 inhibit-splash-screen t
 initial-scratch-message nil
 package--init-file-ensured t
 initial-major-mode 'ielm
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
  :ensure t
  :defer t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; displays the key bindings following your currently entered incomplete command
(use-package which-key
  :ensure t
  :init
  (setq which-key-side-window-max-height 0.40)
  (which-key-mode))

(use-package quelpa-use-package
  :defer t
  :ensure t
  :init
    (setq quelpa-checkout-melpa-p nil)
    (setq quelpa-update-melpa-p nil))

(require 'quelpa-use-package)

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
  :ensure t
  :bind (("C-c TAB" . elispm:reformat-buffer)
         ("C-c C-k" . elispm:kill-other-buffers)
         ("C-l a" . elispm:ask-emacs)
         ("C-c t" . elispm:toggle-tab-width))
  :quelpa (elisp-misc :fetcher url :url "https://raw.githubusercontent.com/njdan5691/elisp-misc/master/elisp-misc.el"))

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

;; (use-package engine-mode
;;   ;; default keymap is bound to "C-x /"
;;   :ensure t
;;   :config
;;   (require 'format-spec)
;;   (progn
;;     (setq engine/browser-function 'eww-browser-url)
;;     (defengine google
;;       "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
;;       :keybinding "g"
;;       :browser 'eww-browse-url)
;;     (defengine gist-github
;;       "https://gist.github.com/search?utf=1&&q=%s"
;;       :keybinding "G"
;;       :browser 'eww-browse-url)
;;     (defengine dicionary
;;       "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
;;       :keybinding "d"
;;       :browser 'eww-browse-url)
;;     (engine-mode 1)))

;; (use-package ps-ccrypt
;;   :ensure t
;;   :quelpa (ps-ccrypt :fetcher github :repo "njdan5691/ps-ccrypt"))


;; (use-package yankpad
;;   :ensure t
;;   :bind
;;   (
;;    :map global-map
;;    ("C-c y" . zz:select-snippet)
;;    :prefix-map zz:yankpad-prefix
;;    :prefix "C-c Y"
;;    ("a" . yankpad-append-category)
;;    ("e" . yankpad-edit)
;;    ("m" . yankpad-map)
;;    ("c" . yankpad-capture-snippet)
;;    ("x" . zz:select-snippet)
;;    ("r" . yankpad-reload)
;;    ("i" . yankpad-insert)
;;    ("s" . yankpad-set-category))
;;   :init
;;   (setq yankpad-file (expand-file-name "~/.emacs.d/yankpad.org"))
;;   :config
;;   (defun zz:select-snippet ()
;;     (interactive)
;;     (unless yankpad-category
;;       (or (yankpad-local-category-to-major-mode)
;;           (yankpad-set-category)))
;;     (let ((name (ivy-read "Snippet:" (yankpad-active-snippets))))
;;       (let ((snippet (assoc name (yankpad-active-snippets))))
;;         (if snippet
;;             (yankpad--run-snippet snippet)
;;           (message (concat "No snippet named " name))))))
;;   :ensure ivy)

;; (use-package major-mode-hydra
;;   :ensure t
;;   :bind
;;   ("<f9>" . major-mode-hydra))




(use-package dired-efap
  :ensure t
  :defer t
  :bind (:map dired-mode-map ("r" . dired-efap)))

(use-package easy-kill
  :ensure t
  :defer t
  :bind (([remap kill-ring-save] . easy-kill))
  :bind (([remap mark-paragraph] . easy-mark)))  

(use-package smex
  :ensure t
  :after ivy
  :commands smex-initialize
  :config (smex-initialize))

(use-package aggressive-indent
  :ensure t
  :hook (c-mode-common . aggressive-indent-mode)
  :diminish aggressive-indent-mode)   

(use-package swiper
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq counsel-grep-swiper-limit 600000)
  (setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("M-i" . counsel-imenu)
         ("M-x" . counsel-M-x)))

(use-package ggtags
  :ensure t
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
                  (ggtags-mode 1))))))

(use-package counsel-gtags
  :ensure t
  :defer t
  :requires swiper)

(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x j" . ivy-kill-line)
         ("C-h b" . counsel-descbinds)
         ("C-h v" . counsel-describe-variable)
         ("M-y" . counsel-yank-pop)
         ("C-x r b" . counsel-bookmark)
         ("C-h f" . counsel-describe-function)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . ivy-kill-line))
         ("C-h s" . counsel-set-variable)
         ("C-h f" . counsel-describe-function))
  :config (setq ivy-use-virtual-buffers t))     

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package julia-mode
  :ensure t
  :defer t
  :interpreter ("julia" . julia-mode))

(use-package counsel
  :ensure t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf))
  :requires swiper)

(use-package auto-complete
  :ensure t
  :diminish
  :config
  (progn
    (setq ac-modes '(cc-mode c-mode c++-mode))
    (ac-config-default)
    (setq ac-delay 0.8)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
   (setq show-paren-mode 1
        electric-pair-mode 1))

(use-package paredit
  :ensure t
  :diminish
  :hook (emacs-lisp-mode . paredit-mode))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\'"
  :config
  (setq lua-indent-level 2))

(use-package recentf-ext
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html$")
  :config
  (defun zz:web-mode-hook ()
    ""
    (setq web-mode-script-padding 0)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2))
  (add-hook 'web-mode-hook 'electric-pair-mode)
  (add-hook 'web-mode-hook 'zz:web-mode-hook))

;; Configuration for packages that are part of emacs installation

(use-package js
  :ensure nil
  :defer t
  :config
  (setq js-indent-level 2))

(use-package cc-mode
  :ensure auto-complete
  :defer t
  :bind (:map c-mode-base-map
              ("C-c z" . zz:indent-with-gnu-indent)
              ("C-c x" . c-mark-function)
              ("M-." . ggtags-find-tag-dwim)
              ("M-?" . ggtags-show-definition)
              ("M-," . ggtags-prev-mark)
              ("M-p" . beginning-of-defun)
              ("M-n" . end-of-defun))
  :config
  (defun zz:indent-with-gnu-indent()
    "Indent the buffer using GNU indent"
    (interactive)
    (setq cmd (concat "indent -st " buffer-file-name))
    (save-excursion
      (delete-trailing-whitespace)
      (shell-command-on-region (point-min) (point-max) cmd (buffer-name))))
  (defun my-c-mode-hook ()
    (electric-pair-mode t)
    (abbrev-mode -1)
    (c-toggle-comment-style nil)
    (setq-local eldoc-echo-area-use-multiline-p t)
    (setq fill-mode 80
          yankpad-category "c-mode"
          c-basic-offset 2
          comment-start "// "
          comment-end ""
          show-trailing-whitespace t
          c-default-style "linux")
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
  :hook (dired-mode . hl-line-mode))

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
  :ensure dired-efap
  :commands (dired-dwim-target-directory)
  :bind (("C-l D" . find-name-dired)
         ("C-c o" . zz:dired-open-file)
         ("C-c C-o" . dired-omit-mode)
         :map dired-mode-map
         ("r" . dired-efap)
         ("s" . zz:dired-switch-edit))
  :config
  (defun zz:dired-switch-edit ()
    (interactive)
    (dired-sort-toggle-or-edit t))
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
         ("M-2" . shell-command)
         ("M-a" . mark-whole-buffer))
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
  :ensure nil
  :defer t
  :config
  (progn
    (setq recentf-exclude '("^/var/folders\.*"
                            "COMMIT_EDITMSG\'"
                            ".*-autoloads\.el\'"
                            "[/\]\elpa/"))
    (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
          recentf-max-saved-items 115)))

(use-package shell
  :ensure nil
  :defer t
  :bind (("C-l $" . shell))
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
  :ensure nil
  :bind (:map zz:my-prefix
              ("c" . compile))

  :config
  (setq nmake-command "linc06nmake"
        compilation-last-buffer nil
        compilation-skip-threshold 2))

(use-package apropos
  :ensure nil
  :bind (:map zz:my-prefix
              ("a" . counsel-apropos))) 

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

(bind-key "<f10>" 'xterm-mouse-mode)  

;; (defun counsel-locate-action-extern (x)
;;   "Use xdg-open shell command on X."
;;   (call-process shell-file-name nil
;;                 nil nil
;;                 shell-command-switch
;;                 (format "xdg-open %s"
;;                         (shell-quote-argument x))))



;; (defun select-all ()
;;   (interactive)
;;   (mark-whole-buffer)
;;   (kill-ring-save (mark) (point)))




(let ((file (expand-file-name "macros.el" user-emacs-directory))
      (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/macros.el"))
  (unless (file-exists-p file)
    (url-copy-file url file)))

;; (let ((file (expand-file-name "yankpad.org" user-emacs-directory))
;;       (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/yankpad.org"))
;;   (unless (file-exists-p file)
;;     (url-copy-file url file)))
                                                                                                                                                                                                                                           
(if (file-directory-p "/home/dan/inc")
    (use-package idev
      ;;:disabled
      :defer t
      :quelpa (idev :fetcher github :repo "njdan5691/idev")
      :bind(
            :map zz:my-prefix
                 ("p" . idev:select-project)
                 ("y" . idev:mr-command)
                 ("f" . idev:fcreate)
                 ("s" . idev:submit)
                 ("w" . idev:choose-mr)
                 ("i" . idev:inc-grep)
                 ("O" . idev:show-off)
                 ("z" . idev:inc-files)
                 ("Z" . idev:base-files)
                 ("g" . idev:sget)
                 ("G" . idev:sget-project-files)
                 ("C" . idev:change-generic)
                 :map dired-mode-map
                 ("C-c d" . idev:dired-sdiff)
                 ("C-c p" . idev:dired-edput)
                 ("C-c e" . idev:dired-edget)
                 ("C-c s" . idev:dired-sget)
                                                 ("C-l c" . compile)       
   
                 :prefix-map idev-project-prefix
                 :prefix "P"
                 ("m" . idev:make-project)
                 ("u" . idev:unfreeze-project)
                 ("f" . idev:freeze-project)
                 ("e" . idev:file-history))))

(defun display-startup-echo-area-message ()
  (message "Initialization Completed"))
