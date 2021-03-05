;;; custom-funcs.el - These are custom functions for my config

;; commentary:
;; - This code is a collection of different functionalities.
;; - Functionalities
;;   - Shortcuts to open different config files with
;;     keybindings
;;   - Function to open the file explorer in the current
;;     directory with a command in Emacs.

;; Opens a config file specified by the key sequence
(defun dk/open-config (&optional dk/open-config-file)
  "open the config.org file"
  (interactive)
  (if (string-equal dk/open-config-file "")
      (switch-to-buffer (find-file-noselect (concat user-emacs-directory "/init.el")));;"~/.emacs.d/init.el"))
    (switch-to-buffer (find-file-noselect (concat "~/.emacs.d/"
						  dk/user-emacs-subdir
						  "/"
						  dk/open-config-file) nil nil t))))

(global-set-key (kbd "C-x RET RET") (lambda () (interactive)(dk/open-config "")))
(global-set-key (kbd "C-x RET o") (lambda () (interactive)(dk/open-config "org-mode.el")))
(global-set-key (kbd "C-x RET c") (lambda () (interactive)(dk/open-config "custom-funcs.el")))
(global-set-key (kbd "C-x RET e") (lambda () (interactive)(dk/open-config "emacs.el")))
(global-set-key (kbd "C-x RET p") (lambda () (interactive)(dk/open-config "programming.el")))

;; Function to opent the file explorer. Currently only Windows is implemented.
(defun explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (when (string-equal system-type "windows-nt")
    (shell-command "explorer .")))

(provide 'custom-funcs.el)
;;; custom-funcs.el ends here



