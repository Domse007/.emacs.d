(defconst dk/variable-file-dir "~/"
  "Directory of the local emacs config variables.")

(defconst dk/variable-file-name ".emacs-config-vars"
  "File name of the local emacs config variables.")

(defun dk/set-config-variables ()
  "Get the directories and save it to the file to save it persistant."
  (let* ((base-path (read-directory-name "Directory at startup: " "~/"))
	 (base-path-ending (dk/check-ends-as-dir base-path))
	 (journal-dir (read-directory-name "Org-roam directory: " base-path))
	 (journal-dir-ending (dk/check-ends-as-dir journal-dir))
	 (roam-dir (read-directory-name "Org-journal directory: " base-path))
	 (roam-dir-ending (dk/check-ends-as-dir roam-dir)))
    (write-region
     (concat "(setq dk/user-system-base-path \""
	     base-path
	     base-path-ending
	     "\")\n(setq dk/org-journal-dir \""
	     journal-dir
	     journal-dir-ending
	     "\")\n(setq dk/org-roam-dir \""
	     roam-dir
	     roam-dir-ending
	     "\")")
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
	(load-file var-file)
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

;; Disable the visual directory selector.
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; Call the function to check if the variables exist.
;; This is called when emacs is started.
(dk/check-config-variables)

(provide 'dk/custom-set-variables)
