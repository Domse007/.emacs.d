(defgroup dk/config nil
  "Group for all custom config variables.")

(defconst dk/config-major-version 0.5
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

(defconst dk/user-emacs-etcdir "var/"
  "Default location for device specific files")

(defconst dk/user-config-file "~/.oec.el"
  "File where the customization by the user is performed.")

(defconst dk/config-core-path (concat user-emacs-directory "core/")
  "Location where the core files are located.")

(defconst dk/config-core-list
  '((:file base-use-package :description "Setup of use-package.")
    (:file base-config :description "Setup of invisible packages.")
    (:file base-emacs :description "Setup of built-in things.")
    (:file base-design :description "Definitions of visible related packages.")
    (:file base-evil :description "Definitions of file definitions."))
  "List of core files for this config.")

(defconst dk/config-optional-path (concat user-emacs-directory "modules/")
  "Default location of config files.")

(defconst dk/config-optional-list
  '((:file custom-search :description "Custom functions for querying config.")
    (:file custom-funcs :description "General custom functions.")
    (:file custom-theme :description "Definitions of theme related packages.")
    (:file custom-helm :description "Setup of helm packages.")
    (:file text-org-mode :description "Setup of org-mode.")
    (:file text-org-spell :description "Setup spell checking.")
    (:file text-org-roam :description "Setup of org-roam.")
    (:file programming-base :description "Setup of basic programming features.")
    (:file programming-rust :description "Setup rust development environment.")
    (:file programming-elisp :description "Setup elisp development environment.")
    (:file programming-python :description "Setup python development environment.")
    (:file programming-haskell :description "Setup haskell development environment."))
  "List of optional files that can be loaded at startup.")

(defvar dk/after-optional-config-hook nil
  "Hook that is run after the user definable modules are loaded.")

(defcustom dk/config-optional-selected-list '()
  "List of symbols that will be `required'. This is customized by the user in the
file "
  :group 'dk/config)

(defconst dk/config-after-init-path (concat user-emacs-directory "after-init/")
  "Default location where the after-init scripts reside.")

(defconst dk/config-after-init-list
  '((:file custom-after-init :description "Setup stuff at end of config init."))
  "List of files that are loaded after optional files are loaded.")

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

(defmacro dk/customize! (module)
  "Add a module to the to be loaded config."
  `(push ,module dk/config-optional-selected-list))

(defmacro dk/theme! (theme)
  `(add-hook 'dk/after-optional-config-hook
             (lambda () (setq dk/theme ,theme))))

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

(add-to-list 'load-path dk/config-core-path)
(add-to-list 'load-path dk/config-optional-path)
(add-to-list 'load-path dk/config-after-init-path)

(defun dk/load-config ()
  "Load the files specified in the `dk/config-core-list',
`dk/config-optional-list' and `dk/config-after-init-list'. It also loads the
file specified in `dk/user-config-file' to see what modules are required."
  (dolist (item dk/config-core-list)
    (let ((file (plist-get item :file)))
      (dk/log 'info "Loading " (symbol-name file) ".")
      (require file)))
  ;; load the user customization.
  (dk/log 'info "Loading the user configuration.")
  (load-file dk/user-config-file)
  (dolist (item dk/config-optional-selected-list)
    (dk/log 'info "Loading " (symbol-name item) ".")
    (require item))
  (run-hooks 'dk/after-optional-config-hook)
  (dk/load-theme) ; load the default theme.
  (dolist (item dk/config-after-init-path)
    (let ((file (plist-get item :file)))
      (dk/log 'infog "Loading " (symbol-name file) ".")
      (require item))))

;; Check the operating system.
(dk/check-system)
;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
