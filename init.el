(defcustom dk/user-system-base-path "~/CloudStation/"
  "Selected path at startup")

(defcustom dk/user-emacs-subdir "modules/"
  "Default location of config files.")

(defcustom dk/user-emacs-etcdir "var/"
  "Default location for device specific files")

(defconst dk/config-file-list
  '("use-package.el"
    "config.el"
    "emacs.el"
    "design.el"
    "helm.el"
    "org-mode.el"
    "org-roam.el"
    "org-journal.el"
    "programming.el"
    "programming-rust.el"
    "programming-elisp.el")
  "List of all files for config.")

(defun load-config ()
  (interactive)
  (dolist (item dk/config-file-list)
    (load-file
     (concat user-emacs-directory
	     dk/user-emacs-subdir
	     item))))

(load-config)
