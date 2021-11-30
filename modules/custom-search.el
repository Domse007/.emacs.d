(defun dk/search-config-file ()
  "Get the user input and call the function to open
the file."
  (interactive)
  (let ((files (dk/search-extract-files))
	(result ""))
    (setq result
	  (helm
	   :sources
	   (helm-build-sync-source "Config files:"
	     :candidates files
	     :fuzzy-match t)
	   :buffer "*helm-config-search*"))
    (when (not (equal (length result) 0))
      (dk/search-open-file result (dk/search-check-prefix-p result)))))

(defun dk/search-open-file (file arg)
  "Open the config file. The arg specifies if
`dk/user-emacs-subdir' should be used."
  (message (concat "Opening: " file))
  (let ((prefix (if arg
		    dk/user-emacs-subdir
		  "")))
    (find-file (concat user-emacs-directory prefix file))))

(defun dk/search-extract-files ()
  "Extract the files from `dk/config-file-list'."
  (let ((res '()))
    (dolist (item dk/config-file-list)
      (push (car item) res))
    res))

(defun dk/search-check-prefix-p (name)
  "Check if the prefix must be applied."
  (let ((res nil))
    (dolist (item dk/config-file-list)
      (let ((file (car item))
	    (arg (cdr item)))
	(when (string-equal file name)
	  (setq res arg))))
    res))

(global-set-key (kbd "C-x RET") 'dk/search-config-file)

(provide 'dk/custom-search)
