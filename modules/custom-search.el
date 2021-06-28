(require 'helm)

(defun search-config-file ()
  "Get a helm buffer to open a specific config file."
  (interactive)
  (let ((files dk/config-file-list)
	(result ""))
    (setq result
	  (helm
	   :sources
	   (helm-build-sync-source "Config files:"
	     :candidates files
	     :fuzzy-match t)
	   :buffer "*config-search*"))
    (if (not (equal (length result) 0))
	(progn (message "Opening: " result)
	       (find-file
		(concat user-emacs-directory
			dk/user-emacs-subdir
			result))))))

(global-set-key (kbd "C-x RET") 'search-config-file)

(defun explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (when (string-equal system-type "windows-nt")
    (shell-command "explorer ."))
  (when (string-equal system-type "gnu/linux")
    (shell-command "xdg-open .")))

(provide 'custom-search.el)
