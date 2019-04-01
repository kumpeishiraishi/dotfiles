(require 'org-install)
(defvar my-config-dir (concat (substitute-in-file-name "$HOME") "/dotfiles/.emacs.d/init_org/"))
(org-babel-load-file (expand-file-name "init.org" my-config-dir))
