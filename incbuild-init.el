(message "Got inbuild-init")
(defun ib:find-file-incnodes ()
	(interactive)
	(find-file "/home/incbuild/incnodes"))

(defun ib:find-file-rawfiles ()
	(interactive)
	(find-file (format "%s/rawfiles" (getenv "OFF"))))

(defun ib:find-file-depot ()
	(interactive)
	(find-file (format "%s" (getenv "DEPOT"))))

(defun ib:find-file-home ()
	(interactive)
	(find-file (format "%s" (getenv "HOME"))))

(defun ib:find-file-off ()
	(interactive)
	(find-file (format "%s" (getenv "OFF"))))

(defun ib:find-file-bin ()
	(interactive)
	(find-file "/home/incbuild/bin"))

(defun ib:dired-update-config ()
	(interactive)
	(dired "/home/incbuild/incinstall/u*.cfg"))

(defun ib:dired-remote-install-config ()
	(interactive)
	(dired "/home/incbuild/incinstall/r*.cfg"))

(bind-keys :map global-map
					 :prefix-map zz:my-prefix
					 :prefix "C-l"
					 ("n" . ib:find-file-incnodes)
					 ("R" . ib:find-file-rawfiles)
           ("O" . ib:find-file-off)
					 ("D" . ib:find-file-depot)
					 ("B" . ib:find-file-bin)
					 ("H" . ib:find-file-home)
					 ("u" . ib:dired-update-config)
					 ("r" . ib:dired-remote-install-config))
