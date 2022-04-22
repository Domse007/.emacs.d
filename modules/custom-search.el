(defconst dk/search-queryable-vars
  `((((:file install) (:file template)) . ,(concat user-emacs-directory "setup/"))
    (((:file init) (:file early-init)) . ,user-emacs-directory)
    (,dk/config-core-list . ,dk/config-core-path)
    (,dk/config-optional-list . ,dk/config-optional-path)
    (,dk/config-after-init-list . ,dk/config-after-init-path))
  "List of cons where the first element is a variable with a list of files. The
second element is a path, where the files can be found. These are used to
easily query the files and open them.")

(defvar dk/search-cache '()
  "List of cons, where the first element is the displayed filename and the
second element is the path to the file.")

(defun dk/search-build-cache ()
  "Build the cache of config files and save it in `dk/search-cache'.
This function is only invoked once."
  (dolist (con dk/search-queryable-vars)
    (let ((listing (car con))
	  (path (cdr con)))
      (dolist (file listing)
	(let ((file-name (symbol-name (plist-get file :file))))
	  (push `(,file-name . ,(concat path file-name ".el"))
		dk/search-cache))))))

(defun dk/search-build-readable-data ()
  "Process `dk/search-cache' to get a list that can be interpreted."
  (let ((c-list '()))
    (dolist (cons-cell dk/search-cache)
      (push (nth 0 cons-cell) c-list))
    c-list))

(defun dk/search-open-selected-file (file)
  "Open the file that was reported by read."
  (dolist (item dk/search-cache)
    (let ((name (car item))
	  (path (cdr item)))
      (when (string-equal name file)
	(dk/log 'info "Opening config file: " name)
	(find-file path)))))

(defun dk/search-config-file ()
  "Main function for the opening of config file. It uses the files specified in
`dk/search-queryable-vars' and caches a more usable form for the function. The
generated cache is stored in `dk/search-cache'. With the help of
`dk/search-build-readable-data', the data will be presented to the user. The
selection will be passed to `dk/search-open-selected-file', which looks for the
path in the cache and opens the file."
  (interactive)
  (when (eq dk/search-cache nil)
    (dk/search-build-cache))
  (dk/search-open-selected-file
   (completing-read "Config files: " (dk/search-build-readable-data) nil t)))

(global-set-key (kbd "C-x RET") 'dk/search-config-file)

(defun dk/open-user-config-file ()
  "Open the file specified `dk/user-config-file'."
  (interactive)
  (dk/log 'info "Opening user config file: " dk/user-config-file ".")
  (find-file dk/user-config-file))

(global-set-key (kbd "C-c RET") 'dk/open-user-config-file)

(provide 'custom-search)
