(defconst dk/config-version "0.2.1"
  "Version of the config. It increases on bigger changes.")

(defcustom dk/user-system-base-path ""
  "Selected path at startup")

(defcustom dk/org-journal-dir ""
  "Default location of journal files.")

(defcustom dk/org-roam-dir ""
  "Default directory of org files that should be indexed by roam.")

(defconst dk/user-emacs-subdir "modules/"
  "Default location of config files.")

(defconst dk/user-emacs-etcdir "var/"
  "Default location for device specific files")

(defconst dk/config-file-list
  '("use-package.el"
    "custom-set-variables.el"
    "config.el"
    "emacs.el"
    "custom-search.el"
    "custom-funcs.el"
    "design.el"
    "helm.el"
    "org-mode.el"
    "org-spell.el"
    "org-roam.el"
    "org-journal.el"
    "programming.el"
    "programming-rust.el"
    "programming-elisp.el"
    "programming-python.el"
    "custom-after-init.el")
  "List of all files for config.")

(defun load-config ()
  (interactive)
  (dolist (item dk/config-file-list)
    (load-file
     (concat user-emacs-directory
	     dk/user-emacs-subdir
	     item))))

(load-config)

(provide 'init.el)
