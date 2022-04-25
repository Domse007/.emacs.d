(defgroup dk/config nil
  "Group for all custom config variables.")

(defconst dk/config-major-version 0.5
  "Major version of the config. It increases on major changes.")

(defconst dk/config-minor-version 1
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

(defconst dk/user-emacs-etcdir "var/"
  "Default location for device specific files")

(defconst dk/user-config-file "~/.oec.el"
  "File where the customization by the user is performed.")

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

(defmacro dk/use-module! (module)
  "Let the user define which modules should be loaded.
The only argument is a symbol with a name of a module."
  `(push ,module dk/user-defined-modules))

(defvar dk/user-defined-modules nil
  "List of module names that are defined with `dk/use-module'.")

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
  (load-file dk/user-config-file)
  (require 'base-module-resolving)
  (dk/resolve-modules)
  (dk/load-modules)
  (dk/run-hooks)
  (message "%s" dk/after-optional-config-hook)
  (run-hooks 'dk/after-optional-config-hook)
  (dk/log 'info "Config loaded."))

;; Check the operating system.
(dk/check-system)
;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
