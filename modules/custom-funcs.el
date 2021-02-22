(defun dk/open-config (&optional dk/open-config-file)
  "open the config.org file"
  (interactive)
  (if (string-equal dk/open-config-file "")
      (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el"))
    (switch-to-buffer (find-file-noselect (concat "~/.emacs.d/"
						  dk/user-emacs-subdir
						  "/"
						  dk/open-config-file) nil nil t))))

(global-set-key (kbd "C-x RET RET") (lambda () (interactive)(dk/open-config "")))
(global-set-key (kbd "C-x RET o") (lambda () (interactive)(dk/open-config "org-mode.el")))
(global-set-key (kbd "C-x RET c") (lambda () (interactive)(dk/open-config "custom-funcs.el")))
(global-set-key (kbd "C-x RET e") (lambda () (interactive)(dk/open-config "emacs.el")))
(global-set-key (kbd "C-x RET p") (lambda () (interactive)(dk/open-config "programming.el")))

(defun explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (when (string-equal system-type "windows-nt")
    (shell-command "explorer .")))

(provide 'custom-funcs.el)



