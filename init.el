(let ((file (expand-file-name "dan-init.el" user-emacs-directory))
      (url "https://raw.githubusercontent.com/njdan5691/emacs-init/master/dan-init.el"))
  (unless (file-exists-p file)
    (url-copy-file url file))
  (load-file file))

