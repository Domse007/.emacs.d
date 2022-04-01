(defconst dk/variable-file-dir (if dk/portable-is-portable
				   (concat user-emacs-directory
					   "../")
				 "~/")
  "Directory of the local emacs config variables.")

(defconst dk/variable-file-name ".emacs-config-vars"
  "File name of the local emacs config variables.")

(defun dk/set-config-variables ()
  "Get the directories and save it to the file to save it persistant."
  (let* ((base-path (read-directory-name "Directory at startup: " "~/"))
	 (base-path-ending (dk/check-ends-as-dir base-path))
	 (roam-dir (read-directory-name "Org-roam directory: " base-path))
	 (roam-dir-ending (dk/check-ends-as-dir roam-dir))
	 (use-40-percent-bindings (y-or-n-p "Use 40 percent keyboard? ")))
    (write-region
     (concat "(setq dk/user-system-base-path \""
	     base-path
	     base-path-ending
	     "\")\n(setq dk/org-roam-dir \""
	     roam-dir
	     roam-dir-ending
	     "\")\n(setq dk/use-40-percent-keyboard "
	     (dk/bool-to-string use-40-percent-bindings)
	     ")")
     nil
     (concat dk/variable-file-dir dk/variable-file-name))))

(defun dk/check-ends-as-dir (path)
  "Checks that a string ends with a forward slash, that it always is recognized
as a directory."
  (let ((last-char (substring path -1)))
    (if (string-equal last-char "/")
	""
      "/")))

(defun dk/check-config-variables ()
  "Check if the file exists. If it does, read it and get the values. If not call
 `dk/set-config-variables'. In the end it calls `dk/install-packages'."
  (let ((var-file (concat dk/variable-file-dir
			  dk/variable-file-name)))
    (if (file-exists-p var-file)
	(progn (load-file var-file)
	       (dk/log "Loaded custom file." 'info))
      (progn
	(dk/set-config-variables)
	(dk/check-config-variables)))))

(defun dk/reset-config-variables ()
  "Reset the variables. This is persistant."
  (interactive)
  (let ((var-file (concat dk/variable-file-dir
			  dk/variable-file-name)))
    (delete-file var-file)
    (dk/set-config-variables)
    (load-file var-file)))

(defun dk/bool-to-string (arg)
  "Convert arg to a string"
  (if arg
      "t"
    "nil"))

;; Disable the visual directory selector.
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; Call the function to check if the variables exist.
;; This is called when emacs is started.
(dk/check-config-variables)

(provide 'base-custom-set-variables)
