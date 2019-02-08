(let ((file (expand-file-name "dan-init.el" user-emacs-directory))
      (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/dan-init.el"))
  (unless (file-exists-p file)
    (url-copy-file url file))
  (load-file file))


;;(let ((file (expand-file-name "macros.el" user-emacs-directory))
;;      (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/macros.el"))
;;  (unless (file-exists-p file)
;;    (url-copy-file url file)))

(use-package idev
  :disabled                                                                                                          
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
        ("g" . idev:sget)
        ("G" . idev:sget-project-files)
        ("C" . idev:change-generic)
        :map dired-mode-map
        ("C-c d" . idev:dired-sdiff)
        ("C-c p" . idev:dired-edput)
        ("C-c s" . idev:dired-sget)
        ("C-l c" . compile)
        ("C-c e" . idev:dired-edget)
        :map zz:my-prefix
        :prefix-map idev-project-prefix
        :prefix "P"
        ("m" . idev:make-project)
        ("u" . idev:unfreeze-project)
        ("f" . idev:freeze-project)
        ("e" . idev:file-history)))


