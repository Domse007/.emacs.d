;; Decrease the garbage collector limit to 8mb after
;; startup to ensure that there are no lags, when the
;; garbage collector kicks in.
(setq gc-cons-threshold 800000)

(defun dk/greet (&optional not-print)
  "Print a nice greeting after startup. If
not-print is given, the function will not print the
message and just return it."
  (interactive)
  (let ((greet (concat "Welcome back! Running GNU/Emacs "
		       emacs-version
		       ". The config is on version "
		       (dk/config-version t t)
		       ".")))
    (if not-print
	greet
      (progn (message greet)
	     greet))))

(setq dashboard-banner-logo-title (dk/greet t))

(when window-system
  (helm-posframe-enable))

(let ((dir (concat user-emacs-directory "auto-save-list")))
  (when (file-directory-p dir)
    (delete-directory dir t nil)))

(provide 'dk/custom-after-init)
