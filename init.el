(defgroup dk/config nil
  "Group for all custom config variables.")

(defconst dk/config-major-version 0.2
  "Major version of the config. It increases on major changes.")

(defconst dk/config-minor-version 2
  "Minor version of the config. It increases on smaller changes.")

(defcustom dk/windows-flag nil
  "Flag that is set, if the host is a windows-nt kernel."
  :type 'bool
  :group 'dk/config)

(defcustom dk/user-system-base-path ""
  "Selected path at startup"
  :type 'string
  :group 'dk/config)

(defcustom dk/org-journal-dir ""
  "Default location of journal files."
  :type 'string
  :group 'dk/config)

(defcustom dk/org-roam-dir ""
  "Default directory of org files that should be indexed by roam."
  :type 'string
  :group 'dk/config)

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
    "rss.el"
    "custom-after-init.el")
  "List of all files for config.")

(defun dk/check-system ()
  "Check if the system-type is `windows-nw'. If true, set 
the flag."
  (when (equal system-type "windows-nt")
    (setq dk/windows-flag t)))

(defun dk/load-config ()
  "Load the files specified in the `dk/config-file-list' list."
  (interactive)
  (let ((path (concat user-emacs-directory dk/user-emacs-subdir)))
    (dolist (item dk/config-file-list)
      (load-file (concat path item)))))

;; Call the entry points of the config.
(dk/check-system)
(dk/load-config)

(provide 'init.el)
