;; Decrease the garbage collector limit to 8mb after
;; startup to ensure that there are no lags, when the
;; garbage collector kicks in.
(setq gc-cons-threshold dk/original-gc-threshold)

;;------------------------------------------------------------------------------

(require 'dashboard)

(defun dk/greet (&optional not-print)
  "Print a nice greeting after startup. If
not-print is given, the function will not print the
message and just return it."
  (interactive)
  (let ((greet (concat "Welcome back! Running GNU/Emacs "
		       emacs-version
		       ;; ". The config is on version "
		       ;; (dk/config-version t t)
		       ".")))
    (if not-print
	greet
      (progn (dk/log greet 'info)
	     greet))))

(setq dashboard-banner-logo-title (dk/greet t))

;;------------------------------------------------------------------------------

(when window-system
  (dk/log "Enabling posframe..." 'info)
  (helm-posframe-enable))

(defun dk/delete-unused-config-dirs (dk/dirs)
  "Delete directories in the .emacs.d folder
that aren't used but still are created."
  (dolist (d dk/dirs)
    (let ((dir (concat user-emacs-directory d)))
      (when (file-directory-p dir)
	(delete-directory dir t nil)))))

(dk/delete-unused-config-dirs '("auto-save-list" "etc"))

;;------------------------------------------------------------------------------

(dk/log "Config is loaded." 'info)

(provide 'custom-after-init)
