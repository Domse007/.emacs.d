(defgroup dk/config nil
  "Group for all custom config variables.")

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

(defcustom dk/get-package-override-git-availability nil
  "Flag that will if true use get-package instead of quelpa, which requires git."
  :type 'bool
  :group 'dk/config)

(defconst dk/default-font "Source Code Pro"
  "The default font that will be used.")

(defconst dk/user-emacs-cache-dir (concat user-emacs-directory ".cache/")
  "Default location for device specific files.")

(defconst dk/config-core-path (concat user-emacs-directory "core/")
  "Location where the core files are located.")

(defconst dk/config-optional-path (concat user-emacs-directory "modules/")
  "Default location of config files.")

;; Macros
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

(defmacro dk/theme! (theme)
  `(add-hook 'dk/after-optional-config-hook
             (lambda () (progn (setq dk/theme ,theme)
                               (dk/load-theme)))))

(defmacro use-modules! (modules)
  "Let the user define which modules should be loaded.
The only argument is a list with symbols of the modules."
  (declare (indent 1))
  `(dolist (module ,modules)
     (require module)))

;; Tracking of external dependencies
;;------------------------------------------------------------------------------

(defvar dk/external-dependencies nil
  "List of external programs that are used with this config.")

(defun new-external-dependency! (program)
  "Add a new external program to `dk/external-dependencies'. It is either a
symbol or a cons. If it is a symbol, it is just the name of the dependency. If
it is a cons cell, the car is the same symbol, but the cdr is a string with
installation instructions."
  (unless (member program dk/external-dependencies)
    (push program dk/external-dependencies)))

;; init functions
;;------------------------------------------------------------------------------

(defun dk/check-system ()
  "Check if the system-type is `windows-nt'. If true, set 
the flag."
  (cond ((string-equal system-type "windows-nt")
	 (setq dk/windows-flag t)
         (dk/log 'info "Detected Windows. Setting variable..."))
	((string-equal system-type "gnu/linux")
	 (setq dk/linux-flag t)
         (dk/log 'info "Detected Linux. Setting variable..."))))

(defun dk/load-config ()
  (load-file (concat dk/config-core-path "base-module-declaration.el"))
  (dk/load-core)
  (load-file (dk/user-config-get-user-file))
  ;; eval function that defines to be loaded modules.
  (dk/user-file-setup)
  ;; load user defined stuff
  (dk/user-file-custom)
  
  (dk/log 'info "Running custom after init hooks.")
  (run-hooks 'dk/custom-after-init-hook)
  
  (run-hooks 'dk/after-optional-config-hook)
  (dk/log 'info "Config loaded.")
  (setq gc-cons-threshold dk/original-gc-threshold)) ; old gc value

;; Check the operating system.
(dk/check-system)
;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
