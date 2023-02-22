(defun dk/open-config-file ()
  "Open a config file that is defined in `dk/modules'."
  (interactive)
  (let* ((modules dk/modules)
	 (files (mapcar #'car modules))
	 (res (completing-read "Config files: " files nil t))
	 (file-to-open nil))
    (dolist (file modules)
      (message "%s %s" res (car file))
      (when (string-equal res (car file))
	(let ((file-name (symbol-name (car file)))
	      (file-location (symbol-name (nth 1 file))))
	  (cond ((string-equal file-location "root")
		 (setq file-to-open (concat file-name ".el")))
		(t
		 (setq file-to-open (concat file-location "/"
					    file-name ".el")))))))
    (dk/log 'info "Opening config file: %s" file-to-open)
    (find-file (expand-file-name file-to-open user-emacs-directory))))

(global-set-key (kbd "C-x RET") 'dk/open-config-file)

(provide 'custom-search)
