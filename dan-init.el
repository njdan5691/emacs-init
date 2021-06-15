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
      (set-frame-font "Consolas-19" nil t)
      ))

(unless (eq window-system 'x)
  (xterm-mouse-mode))

;; If the variable is a buffer-local, and you want it the
;; same in all types of buffers add it here.
(setq-default
 tab-width 2
 indent-tabs-mode t)

(setq
 inhibit-splash-screen t
 confirm-kill-processes nil
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
  (setq which-key-side-window-max-height 0.50)
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
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-z")       'save-buffers-kill-emacs)

(use-package elisp-misc
  :ensure t
  :bind (:map zz:my-prefix
              ("TAB" . elispm:reformat-buffer)
              ("k" . elispm:kill-other-buffers)
              ("h" . elispm:find-file-hints)
              ("x" . elm:remove-lines)
              ("t" . elispm:toggle-tab-width))

  :quelpa (elisp-misc :fetcher url
                      :url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/elisp-misc.el"))

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

(use-package magit
  :bind (("C-x g" . magit-status))
  :ensure t)

(use-package ivy
  :ensure t
  :bind (:map zz:my-prefix
              ("r" . counsel-rg)
              ("f" . counsel-fzf)
              ("R" . counsel-recentf))

  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap bookmark-jump] . counsel-bookmark)
         ([remap yank-pop] . counsel-yank-pop)
         ("C-h s" . counsel-set-variable))
  :config (setq ivy-use-virtual-buffers t))

(use-package swiper
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq counsel-grep-swiper-limit 600000)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper)
         ("C-l o" . counsel-find-file)
         ("M-i" . counsel-imenu)
         ("M-x" . counsel-M-x)))

;;
;; (use-package counsel
;;   :ensure t
;;   :requires swiper)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package julia-mode
  :ensure t
  :defer t
  :interpreter ("julia" . julia-mode))

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

(use-package ivy-rich
  :ensure t
  :config (ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'abbreviate
        ivy-rich-path-style 'abbrev )
  ;; use buffer-file-name instead of default-directory or list-buffers-directory
  ;; so that special buffers, e.g. *scratch* don't get a directory (we return nil in those cases)
  (defun ivy-rich--switch-buffer-directory (candidate)
    (let* ((buffer (get-buffer candidate))
           (fn (buffer-file-name buffer)))
      ;; if valid filename, i.e. buffer visiting file, return containing directory
      ;; if dired visiting directory, return that directory
      ;; else return nil
      (if fn (directory-file-name fn) (buffer-local-value 'dired-directory buffer))))

  ;; override ivy-rich project root finding to use FFIP or to skip completely
  (defun ivy-rich-switch-buffer-root (candidate)
    ;; 1. changed let* to when-let*; if our directory func above returns nil,
    ;;    we don't want to try and find project root
    (when-let* ((dir (ivy-rich--switch-buffer-directory candidate)))
      (unless (or (and (file-remote-p dir)
                       (not ivy-rich-parse-remote-buffer))
                  ;; Workaround for `browse-url-emacs' buffers , it changes
                  ;; `default-directory' to "http://" (#25)
                  (string-match "https?://" dir))
        (cond
         ;; 2. replace the project-root-finding
         ;; a. add FFIP for projectile-less project-root finding (on my setup much faster) ...
         ((require 'find-file-in-project nil t)
          (let ((default-directory dir))
            (ffip-project-root)))
         ;; b. OR disable project-root-finding altogether
         (t "")
         ((bound-and-true-p projectile-mode)
          (let ((project (or (ivy-rich--local-values
                              candidate 'projectile-project-root)
                             (projectile-project-root dir))))
            (unless (string= project "-")
              project)))
         ((require 'project nil t)
          (when-let ((project (project-current nil dir)))
            (car (project-roots project))))
         )))))


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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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
  :bind (
         :map dired-mode-map
              ("C-c o" . zz:dired-open-file)
              ("C-c c" . dired-do-compress))
   :bind (
         :map dired-mode-map
              :prefix-map zz:my-dired-prefix
              :prefix "z"
              ("o" . dired-omit-mode)
              ("x" . zz:dired-open-file)
              ("d" . find-name-dired)
              ("r" . dired-efap)
              ("s" . zz:dired-switch-edit))
  :config
    (defvar dired-compress-files-alist
      '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
        ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
        ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
        ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
        ("\\.zip\\'" . "zip %o -r --filesync %i")
        ("\\.7z\\'" . "7z a %o %i")
        )
      "Control the compression shell command for `dired-do-compress-to'.                                                          
                                                                                                                                  
Each element is (REGEXP . CMD), where REGEXP is the name of the                                                                   
archive to which you want to compress, and CMD is the                                                                             
corresponding command.                                                                                                            
                                                                                                                                  
Within CMD, %i denotes the input file(s), and %o denotes the                                                                      
output file. %i path(s) are relative, while %o is absolute.")

  (defun zz:dired-switch-edit ()
    (interactive)
    (dired-sort-toggle-or-edit t))
  (defun zz:dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (setq dired-listing-switches "-alGhv --group-directories-first"
        dired-copy-preserve-time nil))


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
  ;; :bind (("<f4>" . grep)
  ;;        ("M-4" . grep))
  :config
  (setq grep-use-null-device nil)
  ;;(grep-apply-setting 'grep-command '("rg --max-depth 1 --vimgrep " . 28))
  (grep-apply-setting 'grep-command '("ag --depth 0 --vimgrep " . 24))
  )


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
  :bind (:map zz:my-prefix
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)))

(use-package saveplace  :ensure nil
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
                            "/elpa/"
                            "[/\]\elpa/"))
    (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
          recentf-max-saved-items 115)))

(use-package shell
  :ensure nil
  :defer t
  :bind (:map zz:my-prefix
              ("$" . shell))
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
  :bind (("C-l l" . load-file))
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


(let ((file (expand-file-name "macros.el" user-emacs-directory))
      (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/macros.el"))
  (unless (file-exists-p file)
    (url-copy-file url file)))

(let ((file (expand-file-name "Hints.txt" user-emacs-directory))
      (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/Hints.txt"))
  (unless (file-exists-p file)
    (url-copy-file url file)))

;; (let ((file (expand-file-name "yankpad.org" user-emacs-directory))
;;       (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/yankpad.org"))
;;   (unless (file-exists-p file)
;;     (url-copy-file url file)))


(if (file-directory-p (concat (getenv "HOME") "/incinstall"))
    (let ((file (expand-file-name "incbuild-init.el" user-emacs-directory))
          (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/incbuild-init.el"))
      (unless (file-exists-p file)
        (url-copy-file url file))
      (load-file file)))

(if (file-directory-p (concat (getenv "HOME") "/inc"))
    (use-package idev
      ;;:disabled
      :defer t
      ;;:quelpa (idev :fetcher github :repo "njdan5691/idev")
      :quelpa (idev :fetcher url :url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/idev.el")
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
            ("e" . idev:file-history))
      ))


(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun display-startup-echo-area-message ()
  (message "Initialization Completed"))

(defun elm:remove-lines (pattern &optional b e)
  (interactive "*sPattern:\nr")
  (save-excursion
      (flush-lines pattern b e)))
