(require 'org-install)
(defvar my-config-dir (concat user-emacs-directory "./../dotfiles/.emacs.d/init_org/"))
(org-babel-load-file (expand-file-name "init.org" my-config-dir))
