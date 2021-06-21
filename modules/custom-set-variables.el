(defconst dk/variable-file-dir "~/"
  "Directory of the local emacs config variables.")

(defconst dk/variable-file-name ".emacs-config-vars"
  "File name of the local emacs config variables.")

(defun dk/set-config-variables ()
  "Get the directories and save it to the file to
   save it persistant."
  (let ((base-path "")
	(journal-dir "")
	(roam-dir ""))
    (setq base-path (read-directory-name "Directory at startup: " "~/"))
    (setq journal-dir (read-directory-name "Org-journal directory: " base-path))
    (setq roam-dir (read-directory-name "Org-roam directory: " base-path))
    (write-region
     (concat "(setq dk/user-system-base-path \""
	     base-path
	     "\")\n(setq dk/org-journal-dir \""
	     journal-dir
	     "\")\n(setq dk/org-roam-dir \""
	     roam-dir
	     "\")")
     nil
     (concat dk/variable-file-dir dk/variable-file-name))))

(defun dk/check-config-variables ()
  "Check if the file exists. If it does,
   read it and get the values. If not 
   call dk/set-config-variables"
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

;; Call the function to check if the variables exist.
;; This is called when emacs is started.
(dk/check-config-variables)

(provide 'custom-set-variables.el)
