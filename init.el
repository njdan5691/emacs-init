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

