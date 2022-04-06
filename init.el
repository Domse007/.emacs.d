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

(defconst dk/portable-env-var "PORTABLE"
  "Name of environment variable that is used to indicate if the emacs instance
should be started in portable mode.")

(defcustom dk/portable-definitions-file (concat user-emacs-directory
                                                "../.emacs-portable")
  "File that contains definitions to make this config portable.
It may contain paths to external programs or additional elisp files."
  :type 'string
  :group 'dk/config)

(defcustom dk/portable-is-portable nil
  "This variable is true, if the portable env var exists.")

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
  '((early-init . nil)
    (init . nil)
    (base-use-package . t)
    (base-custom-set-variables . t)
    (base-config . t)
    (base-emacs . t)
    (base-design . t)
    (base-evil . t)
    (custom-search . t)
    (custom-funcs . t)
    (custom-theme . t)
    (custom-helm . t)
    (text-org-mode . t)
    (text-org-spell . t)
    (text-org-roam . t)
    (programming-base . t)
    (programming-rust . t)
    (programming-elisp . t)
    (programming-python . t)
    (programming-haskell . t)
    (custom-after-init . t)
    )
  "List of all files for config. The fist arg is the file
name and the second arg is if the `dk/load-config' function
should load it. Additionally, it specifies if the custom
search function should add the `dk/user-emacs-subdir' prefix.")

;; logger setup
;;------------------------------------------------------------------------------

(defmacro dk/log (kind &rest msg)
  "Log a message. It will report to the minibuffer. The history is available in
the *Messages* buffer."
  `(cond ((eq ,kind 'info)
          (message (concat "[INFO] " ,@msg)))
         ((eq ,kind 'warning)
          (message (concat "[WARNING] " ,@msg)))
         ((eq ,kind 'error)
          (message (concat "[ERROR] " ,@msg)))))

;; portable setup
;;------------------------------------------------------------------------------

(defun dk/check-and-load-portable-file ()
  "Check if the config should be loaded in portable mode and if so, load the
portable file."
  (if (eq (getenv dk/portable-env-var) "1")
      (progn (dk/log 'info "Portable config. Loading portable definitions file.")
             (load-file dk/portable-definitions-file)
             (setq dk/portable-is-portable t))
    (dk/log 'info "Emacs is locally installed.")))

(dk/check-and-load-portable-file)

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
	 (dk/log 'info "Detected Windows. Setting variable..."))
	(dk/linux-flag
	 (dk/log 'info "Detected Linux. Setting variable..."))))

(defcustom dk/loaded-files-counter 0
  "Counter for all loaded config files."
  :type 'number
  :group 'dk/config)

(add-to-list 'load-path (concat user-emacs-directory dk/user-emacs-subdir))

(defun dk/load-config ()
  "Load the files specified in the `dk/config-file-list'"
  (dolist (item dk/config-file-list)
    (let ((file (car item))
          (arg (cdr item)))
      (when arg
        (progn (require file)
               (setq dk/loaded-files-counter (+ dk/loaded-files-counter 1))
               (dk/log 'info "Loading " (symbol-name file) "."))))))

(defun dk/reload-config ()
  "Reload the config after making changes."
  (interactive)
  (dk/load-config)
  (run-hooks 'after-init-hook 'emacs-startup-hook))

;; Check the operating system.
(dk/check-system)
;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
