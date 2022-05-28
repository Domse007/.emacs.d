(defconst dk/minmal-required-emacs-version "27.1"
  "Minimal version supported by this config.")

(defconst dk/config-major-version 0.5
  "Major version of the config. It increases on major changes.")

(defconst dk/config-minor-version 99
  "Minor version of the config. It increases on smaller changes.")

(defconst dk/config-required-user-file-version 1
  "Version of the `dk/user-config-file' file. If the version declared in the
file is smaller, an error will be raised. This version is updated every time
there is a breaking change to the `dk/user-config-file'")

(defun dk/check-required-version-user-file ()
  "Check the version of the user-file and if smaller, error."
  (let ((required-version dk/config-required-user-file-version)
	(declared-version dk/user-file-version))
    (unless (eq required-version declared-version)
      (user-error "User file must have version %s but got %s."
		  required-version declared-version)))
  t)

(defun dk/check-emacs-version ()
  (when (version< emacs-version "27.1")
    (user-error "This version of emacs is too old to work with this config."))
  t)

(when (boundp 'dk/user-file-version) ;; hack to check if whole config is loaded.
  (dk/check-required-version-user-file)
  (dk/check-emacs-version))

(provide 'base-version)