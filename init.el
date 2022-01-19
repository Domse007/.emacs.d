(defgroup dk/config nil
  "Group for all custom config variables.")

(defconst dk/config-major-version 0.4
  "Major version of the config. It increases on major changes.")

(defconst dk/config-minor-version 0
  "Minor version of the config. It increases on smaller changes.")

(defcustom dk/windows-flag nil
  "Flag that is set, if the host is a windows-nt kernel."
  :type 'bool
  :group 'dk/config)

(defcustom dk/linux-flag nil
  "Flag that is set, if the host has a linux kernel."
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

(defcustom dk/org-export-dir ""
  "Default directory where org files are exported to."
  :type 'string
  :group 'dk/config)

(defcustom dk/use-40-percent-keyboard nil
  "Flag that specifies if 40% keyboard specific keybindings should be enabled."
  :type 'bool
  :group 'dk/config)

(defconst dk/default-font "Source Code Pro"
  "The default font that will be used.")

(defconst dk/user-emacs-subdir "modules/"
  "Default location of config files.")

(defconst dk/user-emacs-etcdir "var/"
  "Default location for device specific files")

(defconst dk/config-file-list
  '(("early-init.el" . nil)
    ("init.el" . nil)
    ("use-package.el" . t)
    ("custom-set-variables.el" . t)
    ("config.el" . t)
    ("emacs.el" . t)
    ("custom-search.el" . t)
    ("design.el" . t)
    ("custom-funcs.el" . t)
    ("custom-theme.el" . t)
    ("helm.el" . t)
    ("org-mode.el" . t)
    ("org-spell.el" . t)
    ("org-roam.el" . t)
    ("org-journal.el" . t)
    ("programming.el" . t)
    ("programming-rust.el" . t)
    ("programming-elisp.el" . t)
    ("programming-python.el" . t)
    ("programming-haskell.el" . t)
    ("rss.el" . t)
    ("custom-after-init.el" . t))
  "List of all files for config. The fist arg is the file
name and the second arg is if the `dk/load-config' function
should load it. Additionally, it specifies if the custom
search function should add the `dk/user-emacs-subdir' prefix.")

;; logger setup
;;------------------------------------------------------------------------------

(defvar dk/logging-list '()
  "List that contain logging messages of the config.")

(defun dk/log (msg &optional p)
  "Log message. A report can be generated with `dk/generate-logging-report'."
  (add-to-list 'dk/logging-list msg t)
  (when p
    (message msg)))

(defun dk/generate-logging-report ()
  "Generate a report on the config loadup."
  (interactive)
  (with-output-to-temp-buffer "*logging-report*"
    (switch-to-buffer "*logging-report*")
    (dolist (msg dk/logging-list)
      (print msg))))

;; init functions
;;------------------------------------------------------------------------------

(defun dk/check-system ()
  "Check if the system-type is `windows-nt'. If true, set 
the flag."
  (cond ((string-equal system-type "windows-nt")
	 (setq dk/windows-flag t))
	((string-equal system-type "gnu/linux")
	 (setq dk/linux-flag t)))
  (cond (dk/windows-flag
	 (dk/log "Detected Windows. Setting variable..."))
	(dk/linux-flag
	 (dk/log "Detected Linux. Setting variable..."))))

(defcustom dk/loaded-files-counter 0
  "Counter for all loaded config files."
  :type 'number
  :group 'dk/config)

(defun dk/load-config ()
  "Load the files specified in the `dk/config-file-list' list."
  (interactive)
  (let ((path (concat user-emacs-directory dk/user-emacs-subdir)))
    (dolist (item dk/config-file-list)
      (let ((file (car item))
	    (arg (cdr item)))
	(when arg
	  (progn 
	    (load-file (concat path file))
	    (setq dk/loaded-files-counter
		  (+ dk/loaded-files-counter 1))))))))

;; Check the operating system.
(dk/check-system)
;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
