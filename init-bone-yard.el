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
